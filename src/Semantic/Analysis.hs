{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Semantic.Analysis where

import qualified ProgramTypes         as PT
import qualified AbstractSyntax       as Absyn
import qualified Semantic.Environment as Env

import           Data.Monoid((<>))
import           Control.Monad
import qualified Data.List       as List
import qualified Data.Symbol     as S
import qualified Data.Map.Strict as Map -- we are use ordering in symbols, so we can't use HashMap
import           Control.Monad.State.Lazy
import           Control.Monad.Reader
import           Control.Monad.Except

-- Translation type
-- will be expanded upon in chapter 7
type TranslateExp = ()

data Expty = Expty { expr :: !TranslateExp
                   , typ  :: !PT.Type
                   } deriving Show

data VarTy = VarTy { var  :: !Env.Entry
                   , expr :: !TranslateExp
                   } deriving Show

varTytoExpTy (VarTy {var = var, expr = expr}) =
  let typ = case var of
        Env.VarEntry {Env.ty = ty}     -> ty
        Env.FunEntry {Env.result = ty} -> ty
  in Expty {expr = expr, typ = typ}

data Translation = Trans { tm   :: !Env.TypeMap
                         , em   :: !Env.EntryMap
                         , uniq :: !Int
                         }  deriving Show

-- grabs a unique value from the Translation state
fresh :: MonadState Translation m => m Int
fresh = do
  trans@(Trans {uniq = u}) <- get
  put (trans {uniq = succ u})
  return u

type MonadTranErr m = (MonadError String m)
type MonadTranS   m = (MonadState  Translation m, MonadTranErr m)
type MonadTranR   m = (MonadReader Translation m, MonadTranErr m)
type MonadTranSIO m = (MonadIO m, MonadTranS m)

runMonadTranS :: Env.TypeMap
              -> Env.EntryMap
              -> (StateT Translation (Except e)) a
              -> Either e (a, Translation)
runMonadTranS tm em f = runExcept (runStateT f trans) -- turn back to runExcept once Unique gets up
  where trans = Trans {tm = tm, em = em, uniq = 0}

transExp :: Env.TypeMap -> Env.EntryMap -> Absyn.Exp -> Expty
transExp tm em absyn =
  case runMonadTranS tm em (transExp' False absyn) of
    Left a          -> error a
    Right (expt,tl) -> expt

transExp' :: MonadTranS m => Bool -> Absyn.Exp -> m Expty
transExp' _ (Absyn.IntLit _ _)    = return (Expty {expr = (), typ = PT.INT})
transExp' _ (Absyn.Nil _)         = return (Expty {expr = (), typ = PT.NIL})
transExp' _ (Absyn.StringLit _ _) = return (Expty {expr = (), typ = PT.STRING})
transExp' _ (Absyn.Var x)         = do
  env <- get
  varTytoExpTy <$> runReaderT (transVar x) env

transExp' inLoop (Absyn.Infix' left x right pos) = case x of
  Absyn.Minus -> handleInfixInt inLoop left right pos
  Absyn.Plus  -> handleInfixInt inLoop left right pos
  Absyn.Times -> handleInfixInt inLoop left right pos
  Absyn.Div   -> handleInfixInt inLoop left right pos
  Absyn.And   -> handleInfixInt inLoop left right pos
  Absyn.Or    -> handleInfixInt inLoop left right pos
  Absyn.Gt    -> handleInfixStrInt inLoop left right pos
  Absyn.Ge    -> handleInfixStrInt inLoop left right pos
  Absyn.Lt    -> handleInfixStrInt inLoop left right pos
  Absyn.Le    -> handleInfixStrInt inLoop left right pos
  Absyn.Eq    -> handleInfixSame inLoop left right pos
  Absyn.Neq   -> handleInfixSame inLoop left right pos

transExp' inLoop (Absyn.Negation val pos) = do
  val' <- transExp' inLoop val
  checkInt val' pos
  return (Expty {expr = (), typ = PT.INT})

transExp' inLoop (Absyn.Sequence [] pos) = return (Expty {expr = (), typ = PT.NIL})
transExp' inLoop (Absyn.Sequence xs pos) = last <$> traverse (transExp' inLoop) xs

transExp' inLoop (Absyn.Break pos)
  | inLoop    = return (Expty {expr = (), typ = PT.NIL})
  | otherwise = throwError (show pos <> " break needs to be used inside a loop")

transExp' inLoop (Absyn.While pred body pos) = do
  pred' <- transExp' inLoop pred
  body' <- transExp' True body
  checkInt pred' pos
  checkNil body' pos
  return (Expty {expr = (), typ = PT.NIL})

transExp' inLoop (Absyn.For var from to body pos) = do
  from' <- transExp' inLoop from
  to'   <- transExp' inLoop to
  Trans {tm = tm, em = em} <- get
  checkInt from' pos
  checkInt to' pos
  body' <- locallyInsert1 (transExp' True body)
                          (var, Env.VarEntry {Env.ty = PT.INT, Env.modifiable = False})
  checkNil body' pos -- the false makes it so if we try to modify it, it errors
  return (Expty {expr = (), typ = typ body'})

transExp' inLoop (Absyn.IfThenElse pred then' else' pos) = do
  pred'  <- transExp' inLoop pred
  then'' <- transExp' inLoop then'
  else'' <- transExp' inLoop else'
  checkInt pred' pos
  checkSame then'' else'' pos
  return (Expty {expr = (), typ = typ then''})

transExp' inLoop (Absyn.IfThen pred then' pos) = do
  pred'  <- transExp' inLoop pred
  then'' <- transExp' inLoop then'
  checkInt pred' pos
  checkNil then'' pos
  return (Expty {expr = (), typ = PT.NIL})

transExp' inLoop (Absyn.Funcall fnSym args pos) = do
  Trans {tm = typeMap, em = envMap} <- get
  case envMap Map.!? fnSym of
    Nothing                 -> throwError (show pos <> " function " <> S.unintern fnSym <> " is not defined")
    Just (Env.VarEntry _ _) -> throwError (show pos <> " variable " <> S.unintern fnSym <> " is not a function")
    Just (Env.FunEntry {Env.formals = formals, Env.result = result}) -> do
      zipWithM_ (\arg formal -> do
                    Expty {typ = argType} <- transExp' inLoop arg
                    checkSameTyp formal argType pos)
                args
                formals
      return (Expty {expr = (), typ = result})

transExp' inLoop (Absyn.Assign var toPut pos) = do
  VarTy {var = envVar} <- get >>= runReaderT (transVar var)
  Expty {typ = toPutType} <- transExp' inLoop toPut
  case envVar of
    Env.FunEntry {}                       -> throwError (show pos <> " can't set a function")
    Env.VarEntry {Env.modifiable = False} -> throwError (show pos <> " variable is not modifiable")
    Env.VarEntry {Env.ty = varType}       -> Expty {expr = (), typ = toPutType}
                                            <$ checkSameTyp varType toPutType pos
transExp' inLoop (Absyn.ArrCreate tyid length content pos) = do
  Trans {tm = typeMap} <- get
  transExp' inLoop length >>= (`checkInt` pos)
  case typeMap Map.!? tyid of
    Nothing -> throwError (show pos <> " array type " <> S.unintern tyid <> " undefined")
    Just (PT.ARRAY typ uniqueId) -> do
      Expty {typ = contentType} <- transExp' inLoop content
      checkSameTyp typ contentType pos
      return (Expty {expr = (), typ = (PT.ARRAY typ uniqueId)})
    Just x -> throwError (show pos <> " " <> S.unintern tyid
                         <> " is not of type array, but of type " <> show x)
transExp' inLoop (Absyn.RecCreate tyid givens pos) = do
  Trans {tm = typeMap} <- get
  case typeMap Map.!? tyid of
    Nothing -> throwError (show pos <> " record " <> S.unintern tyid <> " undefined")
    Just (PT.RECORD syms uniqueType) -> do
      let sortedRecType = List.sortOn fst syms -- with these two sorted, we can just compare
          sortedGivens  = List.sortOn Absyn.fieldTyp givens -- and error from there
      sortedGivenTypes <- traverse (\ (Absyn.Field sym exp pos) -> do
                                       Expty {typ = exType} <- transExp' inLoop exp
                                       return (sym, exType, pos)
                                   ) sortedGivens
      zipWithM_ (\ (_, recType)
                   (_, givenType, p) -> checkSameTyp recType givenType p)
                sortedRecType
                sortedGivenTypes
      return (Expty {expr = (), typ = PT.RECORD syms uniqueType})
    Just x -> throwError (show pos <> " " <> S.unintern tyid
                         <> " is not of type record, but of type " <> show x)
-- fix this case... should pop the changes made by transDec
-- and this case should be able to handle mutally recursive functions
-- do this by filtering out TypeDec for add TypeDec
-- and grab the rest by doing the opposite filter, and then just call traverse endVal!
transExp' inLoop (Absyn.Let decs exps pos) = do
--  let typeDec 
  traverse transDec decs
  case exps of
    []   -> return (Expty {expr = (), typ = PT.NIL})
    exps -> last <$> traverse (transExp' inLoop) exps


-- transVar doesn't go to Dec, so it's a reader
transVar :: MonadTranR m => Absyn.Var -> m VarTy
transVar = undefined

transDec :: MonadTranS m => Absyn.Dec -> m ()
transDec = undefined

transTy :: Env.EntryMap -> Absyn.Ty -> PT.Type
transTy = undefined

-- Helper functions----------------------------------------------------------------------------
-- adds a symbol to the envEntry replacing what is there for this scope
locallyInsert1 :: MonadTranS m => m b -> (S.Symbol, Env.Entry) -> m b
locallyInsert1 expression (symb, envEntry) = do
  Trans {tm = typeMap, em = envMap} <- get
  let val = envMap Map.!? symb
  changeEnvValue symb (Just envEntry)

  expResult <- expression

  changeEnvValue symb val

  return expResult

-- could just be foldr locallyInsert1... however I don't trust my reasoning enough to do that
-- adds symbols to the envEntry replacing what is there for this scope
locallyInsert :: MonadTranS m => m b -> [(S.Symbol, Env.Entry)] -> m b
locallyInsert expression xs = do
  Trans {tm = typeMap, em = envMap} <- get
  let vals = fmap (\(symb,_) -> (symb, envMap Map.!? symb)) xs

  traverse (\(symb, envEntry) -> changeEnvValue symb (Just envEntry)) xs

  expResult <- expression

  traverse (uncurry changeEnvValue) vals
  return expResult

-- Changes the Envrionment value... removing a value if there is none, else places the new value in the map
changeEnvValue :: MonadTranS m => S.Symbol -> Maybe Env.Entry -> m ()
changeEnvValue symb val = do
  trans@(Trans {em = envMap}) <- get
  case val of
    Nothing   -> put (trans {em = Map.delete symb envMap})
    Just val' -> put (trans {em = Map.insert symb val' envMap})

-- this function will eventually become deprecated once we handle the intermediate stage
handleInfixExp :: MonadTranS m
               => (Expty -> Absyn.Pos -> m ()) -- A function like checkInt
               -> Bool                         -- whether we are inside a loop or not
               -> Absyn.Exp                    -- left side of infix
               -> Absyn.Exp                    -- right side of infix
               -> Absyn.Pos                    -- the Posiiton
               -> m Expty
handleInfixExp f inLoop left right pos = do
  left'  <- transExp' inLoop left
  right' <- transExp' inLoop right
  f left' pos
  f right' pos
  return (Expty {expr = (), typ = PT.INT})

-- will become deprecated once we handle the intermediate stage
handleInfixSame :: MonadTranS m => Bool -> Absyn.Exp -> Absyn.Exp -> Absyn.Pos -> m Expty
handleInfixSame inLoop left right pos = do
  left'  <- transExp' inLoop left
  right' <- transExp' inLoop right
  checkSame left' right' pos
  return (Expty {expr = (), typ = PT.INT})

handleInfixInt, handleInfixStrInt, handleInfixStr
  :: MonadTranS m => Bool -> Absyn.Exp -> Absyn.Exp -> Absyn.Pos -> m Expty
handleInfixInt    = handleInfixExp checkInt
handleInfixStr    = handleInfixExp checkStr
handleInfixStrInt = handleInfixExp checkStrInt


-- Check checks whether the arguments are of the correct type if not throw a monadError
checkInt :: (MonadTranErr m, Show a) => Expty -> a -> m ()
checkInt (Expty {typ = PT.INT}) pos = return  ()
checkInt (Expty {typ = _})      pos = throwError (show pos <> " integer required")

checkNil :: (MonadTranErr m, Show a) => Expty -> a -> m ()
checkNil (Expty {typ = PT.NIL}) pos = return ()
checkNil (Expty {typ = _})      pos = throwError (show pos <> " null required")

checkArr :: (MonadTranErr m, Show a) => Expty -> a -> m PT.Type
checkArr (Expty {typ = PT.ARRAY typ _}) pos = return typ
checkArr (Expty {typ = _})              pos = throwError (show pos <> " Array type required")

checkStr :: (MonadTranErr m, Show a) => Expty -> a -> m ()
checkStr (Expty {typ = PT.STRING}) pos = return  ()
checkStr (Expty {typ = _})         pos = throwError (show pos <> " string required")

checkStrInt :: (MonadTranErr m, Show a) => Expty -> a -> m ()
checkStrInt (Expty {typ = PT.STRING}) pos = return  ()
checkStrInt (Expty {typ = PT.INT})    pos = return  ()
checkStrInt (Expty {typ = _})         pos = throwError (show pos <> " integer or string required")

checkSame :: (MonadTranErr m, Show a) => Expty -> Expty -> a -> m ()
checkSame (Expty {typ = x}) (Expty {typ = y}) = checkSameTyp x y


-- A variant of checkSame that works on PT.Type
checkSameTyp :: (MonadTranErr m, Show a) => PT.Type -> PT.Type -> a -> m ()
checkSameTyp x y pos
  | x == y    = return ()
  | otherwise = throwError (show pos <> " given a " <> show x <> " needs to be the same type as " <> show y)

isTypeDeclaration :: Absyn.Dec -> Bool
isTypeDeclaration (Absyn.TypeDec _ _ _) = True
isTypeDeclaration _                     = False
