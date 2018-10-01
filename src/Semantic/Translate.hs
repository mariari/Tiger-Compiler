{-# LANGUAGE TemplateHaskell #-}

module Semantic.Translate
  ( Access
  , Level
  , outerMost
  , newLevel
  , allocLocal
  ) where

import qualified Frame.X86        as F
import qualified Semantic.Temp    as Temp
import qualified Semantic.IR.Tree as Tree

import Control.Lens hiding(Level)
import Control.Monad.Except

type Escape = Bool

data Level = TopLevel
           | Level { _parent :: Level
                   , _frame  :: F.Frame
                   } deriving Show
makeLenses ''Level

data Access = Access { _level  :: Level
                     , _fAcess :: F.Access
                     } deriving Show
makeLenses ''Access

-- for conversion between the Absyn and the IR
data Exp = Ex Tree.Exp  -- Expression
         | Nx Tree.Stmt -- No Result
         | Cx (Temp.Label -> Temp.Label -> Tree.Stmt) -- Conditional

outerMost :: Level
outerMost = TopLevel

newLevel :: Level -> Temp.Label -> [Escape] -> IO Level
newLevel parent name formals = Level parent <$> F.newFrame name formals

allocLocal :: (MonadIO m, MonadError String m) => Level -> Bool -> m Access
allocLocal TopLevel _ = throwError ("Tried to allocate on the top level")
allocLocal lvl esc = do
  (fra, access) <- liftIO (F.allocLocal (_frame lvl) esc)
  return (Access (set frame fra lvl) access)

formals :: Level -> [Access]
formals TopLevel = []
formals lvl      = Access lvl <$> F.formals (_frame lvl)

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
