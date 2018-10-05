{-# LANGUAGE TemplateHaskell #-}

module Semantic.Translate
  ( Access
  , Level
  , outerMost
  , mainLevel
  , newLevel
  , allocLocal
  , simpleVar
  ) where

import qualified Frame.X86        as F
import qualified Semantic.Temp    as Temp
import qualified Semantic.IR.Tree as Tree

import Control.Lens hiding(Level)
import Data.Unique.Show
import Control.Monad.Except
import Data.Monoid((<>))

type Escape = Bool

data Level = TopLevel
           | Level { _parent :: Level
                   , _frame  :: F.Frame
                   , _unique :: Unique
                   } deriving Show
makeLenses ''Level

data Access = Access { _level  :: Level    -- the level where the access was made
                     , _fAcess :: F.Access -- location of var on the level
                     } deriving Show
makeLenses ''Access

-- for conversion between the Absyn and the IR
data Exp = Ex Tree.Exp  -- Expression
         | Nx Tree.Stmt -- No Result
         | Cx (Temp.Label -> Temp.Label -> Tree.Stmt) -- Conditional

outerMost :: Level
outerMost = TopLevel

mainLevel :: IO Level
mainLevel = newLevel outerMost (Temp.nameLabel "Tiger_Main") []

-- True represents the static link added to the formals
newLevel :: Level -> Temp.Label -> [Escape] -> IO Level
newLevel parent name formals =
  Level parent <$> (F.newFrame name (True : formals))
               <*> newUnique

allocLocal :: (MonadIO m, MonadError String m) => Level -> Bool -> m (Level, Access)
allocLocal TopLevel _ = throwError ("Tried to allocate on the top level")
allocLocal lvl esc = do
  (fra, access) <- liftIO (F.allocLocal (_frame lvl) esc)
  let newLevel = (set frame fra lvl)
  return (newLevel, Access newLevel access)

-- tail to remove the static link
formals :: Level -> [Access]
formals TopLevel = []
formals lvl      = tail $ Access lvl <$> F.formals (_frame lvl)

staticLink :: (MonadError String m) => Level -> Level -> m Tree.Exp
staticLink curr@(Level {}) var@(Level {_unique = uniqVar})
  | _unique curr == uniqVar = return (Tree.Temp F.fp)
  | otherwise               = case F.formals (_frame curr) of
                                []     -> throwError " static link can't be found in formals "
                                link:_ -> F.exp link <$> staticLink (_parent curr) var
staticLink TopLevel _ = throwError " staticLink was passed a TopLevel!!!"
staticLink _ TopLevel = throwError " staticLink was passed a TopLevel!!!"
-- Exp conversions-------------------------------------------------------------------------

exSeq :: [Tree.Stmt] -> Tree.Stmt
exSeq [] = Tree.Exp (Tree.Const 0)
exSeq xs = foldr1 Tree.Seq xs

unEx :: Exp -> IO Tree.Exp
unEx (Ex e) = return e
unEx (Nx s) = return $ Tree.ESeq s (Tree.Const 0)
unEx (Cx genstmt) = do
  r <- Temp.newTemp
  t <- Temp.newLabel
  f <- Temp.newLabel
  return $ Tree.ESeq (exSeq [ Tree.Move (Tree.Temp r) (Tree.Const 1)
                            , genstmt t f
                            , Tree.Label f
                            , Tree.Move (Tree.Temp r) (Tree.Const 0)
                            , Tree.Label t
                            ])
                     (Tree.Temp r)

unNx (Nx s) = return s
unNx (Ex e) = return $ Tree.Exp e
unNx (Cx genstmt) = do
  t <- Temp.newLabel
  f <- Temp.newLabel
  return $ exSeq [ genstmt t f, Tree.Label f, Tree.Label t ]

unCx :: (MonadError String m) => Exp -> Temp.Label -> Temp.Label -> m Tree.Stmt
unCx (Cx x)              t f = return $ x t f
unCx (Ex (Tree.Const 0)) t f = return $ Tree.Jump (Tree.Name f) [f] -- 0 for false
unCx (Ex (Tree.Const _)) t f = return $ Tree.Jump (Tree.Name t) [t] -- others for true!
unCx (Ex e)              t f = return $ Tree.CJump Tree.Ne (Tree.Const 0) e t f
unCx (Nx _)              _ _ = throwError ( " The impossible happened!"
                                         <> " Translate.unCx received an Nx!" )

-- translation of Abstract Syntax into Exp ----------------------------------------------------------------

simpleVar :: (MonadError String m) => Access -> Level -> m Exp
simpleVar (Access varLvl varAccess) currLvl =
  Ex . F.exp varAccess <$> (staticLink currLvl varLvl)

-- Check memory
subscript :: Exp -> Exp -> IO Exp
subscript arrExp lookupExp = trans <$> unEx arrExp <*> unEx lookupExp
  where
    trans unArr unLookup =
      Ex
      . Tree.Mem
      . Tree.Binop (Tree.Mem unArr) Tree.Plus
      . Tree.Binop (Tree.Mem unLookup) Tree.Mul
      $ Tree.Const F.wordSize
