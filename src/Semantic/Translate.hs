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

import qualified Frame.CurrentMachine as F
import qualified Semantic.Temp        as Temp
import qualified Semantic.IR.Tree     as Tree
import qualified AbstractSyntax       as Abs
import           App.Environment
import           Semantic.Fragment

import Prelude hiding (sequence)
import Text.Show.Functions
import Data.IORef
import Data.Unique.Show
import Control.Monad.Except hiding (sequence)
import Control.Monad.Reader hiding (sequence)
import Data.Semigroup ((<>))
import Data.Symbol    (unintern,Symbol)
import Data.List      (elemIndex, find)
import Control.Monad  (zipWithM)
import Control.Lens hiding(Level,assign)

type Escape = Bool

data Level = TopLevel
           | Level { _parent :: Level
                   , _frame  :: F.Frame
                   , _unique :: Unique
                   } deriving Show
makeLenses ''Level

type EnvHasRegs env m = (HasRegs env F.Regs, MonadReader env m)
type EnvHasFrag env m = (MonadReader env m, HasFrag env (IORef [Frag]))

data Access = Access { _level  :: Level    -- the level where the access was made
                     , _fAcess :: F.Access -- location of var on the level
                     } deriving Show
makeLenses ''Access

-- for conversion between the Absyn and the IR
data Exp = Ex Tree.Exp  -- Expression
         | Nx Tree.Stmt -- No Result
         | Cx (Temp.Label -> Temp.Label -> Tree.Stmt) -- Conditional
         deriving Show

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
formals lvl      = tail (Access lvl <$> F.formals (_frame lvl))

staticLink :: (EnvHasRegs env m, MonadError String m) => Level -> Level -> m Tree.Exp
staticLink TopLevel _ = throwError " staticLink was passed a TopLevel!!!"
staticLink _ TopLevel = throwError " staticLink was passed a TopLevel!!!"

staticLink curr@(Level {}) var@(Level {_unique = uniqVar})
  | _unique curr == uniqVar = do
      env <- ask
      return (Tree.Temp (view (regs . F.fp) env))
  | otherwise =
    case F.formals (_frame curr) of
      []     -> throwError " static link can't be found in formals "
      link:_ -> F.exp link <$> staticLink (_parent curr) var
-- Exp conversions-------------------------------------------------------------------------

exSeq :: [Tree.Stmt] -> Tree.Stmt
exSeq [] = Tree.Exp (Tree.Const 0)
exSeq xs = foldr1 Tree.Seq xs

unEx :: Exp -> IO Tree.Exp
unEx (Ex e)       = pure e
unEx (Nx s)       = pure $ Tree.ESeq s (Tree.Const 0)
unEx (Cx genstmt) = trans <$> Temp.newTemp <*> Temp.newLabel <*> Temp.newLabel
  where
    trans r t f =
      Tree.ESeq (exSeq [ Tree.Move (Tree.Temp r) (Tree.Const 1)
                       , genstmt t f
                       , Tree.Label f
                       , Tree.Move (Tree.Temp r) (Tree.Const 0)
                       , Tree.Label t
                       ])
                (Tree.Temp r)

unNx (Nx s)       = pure s
unNx (Ex e)       = pure $ Tree.Exp e
unNx (Cx genstmt) = trans <$> Temp.newLabel <*> Temp.newLabel
  where
    trans t f = exSeq [ genstmt t f, Tree.Label f, Tree.Label t ]

unCx :: (MonadError String m) => Exp -> Temp.Label -> Temp.Label -> m Tree.Stmt
unCx (Cx x)              t f = return $ x t f
unCx (Ex (Tree.Const 0)) t f = return $ Tree.Jump (Tree.Name f) [f] -- 0 for false
unCx (Ex (Tree.Const _)) t f = return $ Tree.Jump (Tree.Name t) [t] -- others for true!
unCx (Ex e)              t f = return $ Tree.CJump Tree.Ne (Tree.Const 0) e t f
unCx (Nx _)              _ _ = throwError ( " The impossible happened!"
                                         <> " Translate.unCx received an Nx!" )

-- this is for when we can't have the monad propogate out and we are **sure** we have no
unCxErr :: Exp -> Temp.Label -> Temp.Label -> Tree.Stmt
unCxErr x t f = case runExcept (unCx x t f) of
                  Right x -> x
                  Left a  -> error a
-- translation of Abstract Syntax into Exp ----------------------------------------------------------------

memPlus :: Tree.Exp -> Tree.Exp -> Tree.Exp
memPlus x = Tree.Mem
          . Tree.Binop x Tree.Plus

simpleVar :: (EnvHasRegs env m, MonadError String m) => Access -> Level -> m Exp
simpleVar (Access varLvl varAccess) currLvl =
  Ex . F.exp varAccess <$> (staticLink currLvl varLvl)

fieldVar :: (MonadError String m, MonadIO m) => Exp -> Symbol -> [(Symbol, b)] -> m Exp
fieldVar record accessor fields =
  case elemIndex accessor (fst <$> fields) of
    Nothing -> throwError ( "fieldVar: field member " <> unintern accessor <> " is not in the record" )
    Just i  -> trans i <$> liftIO (unEx record)
  where
    trans index unRecord = Ex
                         . memPlus (Tree.Mem unRecord)
                         . Tree.Binop (Tree.Const index) Tree.Mul
                         $ Tree.Const F.wordSize

-- Check memory
subscript :: Exp -> Exp -> IO Exp
subscript arrExp lookupExp = trans <$> unEx arrExp <*> unEx lookupExp
  where
    trans unArr unLookup =
      Ex
      . memPlus (Tree.Mem unArr)
      . Tree.Binop (Tree.Mem unLookup) Tree.Mul
      $ Tree.Const F.wordSize

infix' left op right = do
  l <- unEx left
  r <- unEx right
  let relOp op = Cx $ Tree.CJump op l r
      binOp op = Ex $ Tree.Binop l op r
  case op of
    Abs.Plus  -> return $ binOp Tree.Plus
    Abs.Minus -> return $ binOp Tree.Minus
    Abs.Times -> return $ binOp Tree.Mul
    Abs.Div   -> return $ binOp Tree.Div
    Abs.And   -> return $ binOp Tree.And
    Abs.Or    -> return $ binOp Tree.Or
    Abs.Eq    -> return $ relOp Tree.Eq
    Abs.Neq   -> return $ relOp Tree.Ne
    Abs.Lt    -> return $ relOp Tree.Lt
    Abs.Le    -> return $ relOp Tree.Le
    Abs.Gt    -> return $ relOp Tree.Gt
    Abs.Ge    -> return $ relOp Tree.Ge

intLit :: Int -> Exp
intLit = Ex . Tree.Const

stringLit :: (EnvHasFrag env m, MonadIO m) => String -> m Exp
stringLit s = do
  env   <- ask
  frags <- liftIO . readIORef $ env^.frag
  case find (locateFrag s) frags of
    Just (Str l _) ->
      return (Ex (Tree.Name l))
    _ -> do
      l <- liftIO Temp.newLabel
      liftIO (modifyIORef' (env^.frag) (Str l s :))
      return (Ex (Tree.Name l))
  where
    locateFrag toFind (Proc {}) = False
    locateFrag toFind (Str _ s) = toFind == s

nil :: Exp
nil = Ex (Tree.Const 0)

ifThen :: (MonadIO m, MonadError String m) => Exp -> Exp -> m Exp
ifThen pred then' = do
  t      <- liftIO Temp.newLabel
  f      <- liftIO Temp.newLabel
  unPred <- unCx pred t f
  unThen <- liftIO (unNx then')
  return $ Nx (exSeq [ unPred
                     , Tree.Label t
                     , unThen
                     , Tree.Label f ])

ifThenElse :: (MonadIO m, MonadError String m) => Exp -> Exp -> Exp -> m Exp
ifThenElse pred then' else' = do
  t      <- liftIO Temp.newLabel
  f      <- liftIO Temp.newLabel
  done   <- liftIO Temp.newLabel
  result <- liftIO Temp.newTemp
  unPred <- unCx pred t f
  let general unThen unElse = -- this should be the general pattern for Nx and Cx
        exSeq [ unPred
              , Tree.Label t
              , unThen
              , Tree.Jump (Tree.Name done) [done]
              , Tree.Label f
              , unElse
              , Tree.Label done
              ]
  -- this whole case thing can probably be optimized better, for example Ι only check for then' and not else'
  -- for Nx this does not matter as then' ∈ Nx ⟹ else' ∈ Nx. however Ex and Cx are tricky
  case then' of
    Nx unThen -> Nx . general unThen <$> liftIO (unNx else')
    Cx unThen -> return $ Cx (\t' f' -> exSeq [ general (unThen t' f') (unCxErr else' t' f') ])
    Ex unThen -> do
      unElse <- liftIO (unEx else')
      return . Ex
             . Tree.ESeq (exSeq [ unPred
                                , Tree.Label t
                                , Tree.Move (Tree.Temp result) unThen
                                , Tree.Jump (Tree.Name done) [done]
                                , Tree.Label f
                                , Tree.Move (Tree.Temp result) unElse
                                , Tree.Label done
                                ])
             $ Tree.Temp result

recCreate :: [Exp] -> IO Exp
recCreate fields = do
  r <- Temp.newTemp
  let init = Tree.Move (Tree.Temp r)
           . F.externalCall "allocRecord"
           $ [Tree.Const (length fields * F.wordSize)]
  seq <- zipWithM (\e i -> Tree.Move (memPlus (Tree.Temp r)
                                              (Tree.Const (i * F.wordSize)))
                          <$> unEx e)
                  fields [0..]
  return $ Ex (Tree.ESeq (exSeq (init : seq)) (Tree.Temp r))

arrCreate :: Exp -> Exp -> IO Exp
arrCreate size init = trans <$> unEx size <*> unEx init
  where
    trans unSize unInit = Ex (F.externalCall "initArray" [unSize, unInit])

negation :: Exp -> IO Tree.Exp
negation = fmap Tree.Neg . unEx

assign :: Exp -> Exp -> IO Exp
assign left right = trans <$> unEx left <*> unEx right
  where
    trans unL unR = Nx (Tree.Move unL unR)

break label = Nx (Tree.Jump (Tree.Name label) [label])

while :: (MonadIO m, MonadError String m) => Exp -> Exp -> Temp.Label -> m Exp
while test body doneL = do
  testL  <- liftIO Temp.newLabel
  bodyL  <- liftIO Temp.newLabel
  unBody <- liftIO $ unNx body
  unTest <- unCx test bodyL doneL
  return . Nx $ exSeq [ Tree.Label testL
                      , unTest
                      , Tree.Label bodyL
                      , unBody
                      , Tree.Jump (Tree.Name testL) [testL]
                      , Tree.Label doneL]

sequence :: [Exp] -> IO Exp
sequence []  = return . Nx . Tree.Exp $ Tree.Const 0
sequence [x] = return x
sequence xs = do
  unInit <- traverse unNx (init xs)
  case last xs of
    Nx s -> return . Nx $ Tree.Seq (exSeq unInit) s
    Ex e -> return . Ex $ Tree.ESeq (exSeq unInit) e
    Cx c -> return . Cx $ \t f -> Tree.Seq (exSeq unInit) (c t f)

funcall :: (EnvHasRegs env m, MonadIO m, MonadError String m)
        => Temp.Label -> Level -> [Exp] -> Level -> m Exp
funcall _       TopLevel _ _          = throwError "Translate funcall was passed a TopLevel"
funcall labelF levelF args currentLvl = do
  unArgs <- liftIO (traverse unEx args)
  case _parent levelF of
    TopLevel -> return . Ex $ F.externalCall (Temp.fromLabel labelF) unArgs
    Level {} -> do
      link   <- staticLink currentLvl (_parent levelF)
      return . Ex
             . Tree.Call (Tree.Name labelF)
             $ link : unArgs
