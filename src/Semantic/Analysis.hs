{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Semantic.Analysis where

import           TigerParser
import qualified ProgramTypes         as PT
import qualified AbstractSyntax       as Absyn
import qualified Semantic.Environment as Env

import           Data.Maybe
import           Data.Monoid((<>))
import qualified Data.IORef      as Ref
import qualified Data.List       as List
import qualified Data.Symbol     as S
import qualified Data.Map.Strict as Map -- we are use ordering in symbols, so we can't use HashMap
import qualified Data.Set        as Set
import           Control.Monad.State.Lazy
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Foldable (traverse_)

type TranslateExp = ()

type RefMap = Map.Map S.Symbol (Ref.IORef (Maybe PT.Type))

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
type MonadTran    m = (MonadState  Translation m, MonadTranErr m, MonadIO m) -- IO is for refs only

runMonadTran :: Env.TypeMap
             -> Env.EntryMap
             -> (StateT Translation (ExceptT e IO)) a
             -> IO (Either e (a, Translation))
runMonadTran tm em f = runExceptT (runStateT f trans)
  where trans = Trans {tm = tm, em = em, uniq = 0}

transExp :: Env.TypeMap -> Env.EntryMap -> Absyn.Exp -> IO Expty
transExp tm em absyn = do
  x <- runMonadTran tm em (transExp' False absyn)
  case x of
    Left a          -> error a
    Right (expt,tl) -> return expt

transExp' :: MonadTran m => Bool -> Absyn.Exp -> m Expty
transExp' _ (Absyn.IntLit _ _)    = return (Expty {expr = (), typ = PT.INT})
transExp' _ (Absyn.Nil _)         = return (Expty {expr = (), typ = PT.NIL})
transExp' _ (Absyn.StringLit _ _) = return (Expty {expr = (), typ = PT.STRING})
transExp' _ (Absyn.Var x)         = varTytoExpTy <$> transVar x

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
  body' <- locallyInsert1 (transExp' True body) -- could replace with a get and put, we if we don't mutate
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
  Trans {em = envMap} <- get
  case envMap Map.!? fnSym of
    Nothing                 -> throwError (show pos <> " function " <> S.unintern fnSym <> " is not defined")
    Just (Env.VarEntry _ _) -> throwError (show pos <> " variable " <> S.unintern fnSym <> " is not a function")
    Just (Env.FunEntry {Env.formals = formals, Env.result = result}) -> do
      zipWithM_ (\arg formal -> do
                    Expty {typ = argType} <- transExp' inLoop arg
                    actualFormal <- liftIO (actualType formal)
                    actualArg    <- liftIO (actualType argType)
                    checkSameTyp actualFormal actualArg pos)
                args
                formals
      return (Expty {expr = (), typ = result})

transExp' inLoop (Absyn.Assign var toPut pos) = do
  VarTy {var = envVar}    <- transVar var
  Expty {typ = toPutType} <- transExp' inLoop toPut
  case envVar of
    Env.FunEntry {}                       -> throwError (show pos <> " can't set a function")
    Env.VarEntry {Env.modifiable = False} -> throwError (show pos <> " variable is not modifiable")
    Env.VarEntry {Env.ty = varType}       -> Expty {expr = (), typ = toPutType}
                                            <$ checkSameTyp varType toPutType pos
transExp' inLoop (Absyn.ArrCreate tyid length content pos) = do
  Trans {tm = typeMap} <- get
  lengthExp            <- transExp' inLoop length
  checkInt lengthExp pos
  arrType <- liftIO $ traverse actualType (typeMap Map.!? tyid)
  case arrType of
    Nothing -> throwError (show pos <> " array type " <> S.unintern tyid <> " undefined")
    Just (PT.ARRAY typ uniqueId) -> do
      Expty {typ = contentType} <- transExp' inLoop content
      checkSameTyp typ contentType pos
      return (Expty {expr = (), typ = (PT.ARRAY typ uniqueId)})
    Just x -> throwError (show pos <> " " <> S.unintern tyid
                         <> " is not of type array, but of type " <> show x)

transExp' inLoop (Absyn.RecCreate tyid givens pos) = do
  Trans {tm = typeMap} <- get
  recType              <- liftIO $ traverse actualType (typeMap Map.!? tyid)
  case recType of
    Nothing -> throwError (show pos <> " record " <> S.unintern tyid <> " undefined")
    Just (PT.RECORD syms uniqueType) -> do
      let sortedRecType = List.sortOn fst syms              -- with these two sorted, we can just compare
          sortedGivens  = List.sortOn Absyn.fieldTyp givens -- and error from there
      sortedGivenTypes <- traverse (\ (Absyn.Field sym exp pos) -> do
                                       Expty {typ = exType} <- transExp' inLoop exp
                                       return (sym, exType, pos))
                                   sortedGivens
      zipWithM_ (\ (_, recType)
                   (_, givenType, p) -> checkSameTyp recType givenType p)
                sortedRecType
                sortedGivenTypes
      return (Expty {expr = (), typ = PT.RECORD syms uniqueType})
    Just x -> throwError (show pos <> " " <> S.unintern tyid
                         <> " is not of type record, but of type " <> show x)

transExp' inLoop (Absyn.Let decs exps pos) = do
  currentEnv <- get
  transDec decs pos
  case exps of
    []   -> return (Expty {expr = (), typ = PT.NIL})
    exps -> do
      expsTyped <- traverse (transExp' inLoop) exps
      put currentEnv
      return (last expsTyped)

transVar :: MonadTran m => Absyn.Var -> m VarTy
transVar (Absyn.SimpleVar sym pos) = do
  Trans {em = envMap} <- get
  case envMap Map.!? sym of
    Nothing ->
      throwError (show pos <> " " <> S.unintern sym <> " is not defined")
    Just (Env.VarEntry {ty, modifiable}) -> do
      actualTy <- liftIO (actualType ty)
      return $ VarTy { var = Env.VarEntry{ty = actualTy, modifiable = modifiable}
                     , expr = ()
                     }
    Just (Env.FunEntry {formals, result}) -> do -- the book would throw an error... might have to change
      actualTy    <- liftIO (actualType result)
      formalTypes <- liftIO (traverse actualType formals)
      return $ VarTy { var  = Env.FunEntry { formals = formalTypes , result  = actualTy}
                     , expr = ()
                     }
-- this is why we need to be in state and not rader
transVar (Absyn.Subscript arrayType expInt pos) = do
  intExpty <- transExp' False expInt -- not going to allow breaking in an array lookup!
  checkInt intExpty pos
  VarTy {var,expr} <- transVar arrayType
  case var of
    Env.FunEntry {} ->
      throwError (show pos <> " tried to do array lookup on a function")
    Env.VarEntry {modifiable, ty} -> do
      actualTy <- liftIO (actualType ty)
      checkArrTyp actualTy pos
      return $ VarTy { var = Env.VarEntry { ty = actualTy , modifiable = modifiable }
                     , expr = expr }

transVar (Absyn.FieldVar recordType field pos) = do
  VarTy {var,expr} <- transVar recordType
  case var of
    Env.FunEntry {} -> throwError (show pos <> " tried to do record lookup on a function")
    Env.VarEntry {modifiable, ty} -> do
      actualTy <- liftIO (actualType ty)
      checkRecType actualTy pos
      return $ VarTy { var = Env.VarEntry { ty = actualTy
                                          , modifiable = modifiable}
                     , expr = expr }

transTy :: MonadTran m => RefMap -> Absyn.Ty -> m (PT.Type, RefMap)
transTy refMap (Absyn.NameTy sym pos) = do
  (refType, refMap) <- getOrCreateRefMap refMap sym
  return (PT.NAME sym refType, refMap)
transTy refMap (Absyn.ArrayTy sym pos) = do
  (refType, refMap) <- getOrCreateRefMap refMap sym
  mtyp <- liftIO (Ref.readIORef refType)
  case mtyp of
    Nothing -> do
      uniqueNum <- fresh
      return ((PT.ARRAY (PT.NAME sym refType) uniqueNum), refMap)
    Just typ -> do
      actualTy <- liftIO (actualType typ)
      case actualTy of
        PT.ARRAY _ num -> return (PT.ARRAY typ num, refMap)
        _              -> (\f -> (PT.ARRAY typ f, refMap)) <$> fresh
transTy refMap (Absyn.RecordTy recs) = do
  (xs,refMap) <- foldM (\ (xs, refMap) (Absyn.FieldDec name tySym pos) -> do
                          (refType, refMap) <- getOrCreateRefMap refMap tySym
                          refValue          <- liftIO (Ref.readIORef refType)
                          return ((name, fromMaybe (PT.NAME tySym refType) refValue) : xs, refMap))
                       ([], refMap) recs
  unique <- fresh
  return (PT.RECORD (reverse xs) unique, refMap)

transDec :: MonadTran m => [Absyn.Dec] -> Absyn.Pos -> m ()
transDec decs pos = do
  transTypeDecs     typeDecs pos
  transFunDecsHead  funDecs  pos
  transVarDecs      varDecs  pos
  transFunDecsBody  funDecs  pos
  where
    typeDecs = filter isTypeDec     decs
    funDecs  = filter isFunctionDec decs
    varDecs  = filter isVarDec      decs

transVarDecs decs pos = traverse f decs
  where
    f (Absyn.VarDec sym mType exp _) = do
      Expty {typ}            <- transExp' False exp
      trans@(Trans {em, tm}) <- get
      let newMap = put (trans { em = Map.insert sym (Env.VarEntry {ty = typ, modifiable = True}) em })
      case mType of
        Nothing  -> newMap
        Just sty -> case tm Map.!? sty of
                     Nothing -> throwError (show pos <> " type " <> S.unintern sty <> " is not defined ")
                     Just x | x == typ -> newMap
                     Just x -> throwError (show pos <> " " <> show typ <> " is not " <> show x )
    f _ = throwError (show pos <> " violated precondition ")

transFunDecsHead :: (MonadTran m, Traversable t, Show a) => t Absyn.Dec -> a -> m ()
transFunDecsHead decs pos = traverse_ f decs
  where
    f (Absyn.FunDec name fields mtype body pos) = do
          trans@(Trans {em, tm}) <- get
          types                  <- traverse (mapFieldDec (\_ x -> x)) fields
          case mtype of
            Nothing      -> put (trans { em = Map.insert name (Env.FunEntry types PT.UNIT) em})
            Just symType ->
              case tm Map.!? symType of
                Nothing -> throwError (show pos <> " type " <> S.unintern symType <> " is undeifned")
                Just x  -> do
                  actualTy <- liftIO (actualType x)
                  put (trans { em = Map.insert name (Env.FunEntry types actualTy) em})
    f _ = throwError (show pos <> " internal precondition violated at transFunDecHead")

transFunDecsBody :: (MonadTran m, Traversable t, Show a) => t Absyn.Dec -> a -> m ()
transFunDecsBody decs pos = traverse_ f decs
  where f (Absyn.FunDec name fields mtype body pos) = do
          trans@(Trans {em, tm}) <- get
          types                  <- traverse (mapFieldDec (\n t -> (n, Env.VarEntry { ty = t
                                                                                    , modifiable = True})))
                                             fields
          Expty {typ} <- locallyInsert (transExp' False body) types
          bodyType    <- liftIO (actualType typ)
          case em Map.!? name of
            Just (Env.FunEntry {result}) -> do
              trueResultType <- liftIO (actualType result)
              checkSameTyp trueResultType bodyType pos
            _ -> throwError (show pos <> " transFunDecsHead did not put the function in the map")
        f _ = throwError (show pos <> " internal precondition violated at transFunDecBody")

transTypeDecs :: MonadTran m => [Absyn.Dec] -> Absyn.Pos -> m ()
transTypeDecs decs pos = foldM handle1 refMap decs >>= allDefined >> handleCycles decs pos
  where
    refMap :: RefMap
    refMap = mempty
    handle1 refMap (Absyn.TypeDec sym ty pos) =
      case refMap Map.!? sym of
        Just ref -> do
          mty <- liftIO (Ref.readIORef ref)
          case mty of
            Just _  -> throwError (show pos <> " multiple type declarations of " <> show sym)
            Nothing -> writeWithRef ref sym ty refMap
        Nothing -> do
          symRef <- liftIO (Ref.newIORef Nothing)
          writeWithRef symRef sym ty refMap
    handle1 refMap _  = throwError (show pos <> " precondition defied, a non-type was sent to transTypeDecs")
    allDefined refMap = traverse f refMap
    f ref = do
      mty <- liftIO (Ref.readIORef ref)
      case mty of
        Nothing -> throwError (show pos <> " not all type variables are defined")
        Just x  -> return x

writeWithRef :: MonadTran m => Ref.IORef (Maybe PT.Type) -> S.Symbol -> Absyn.Ty -> RefMap -> m RefMap
writeWithRef symRef sym ty refMap = do
  (tType, nRefMap) <- transTy (Map.insert sym symRef refMap) ty
  liftIO (Ref.writeIORef symRef (Just tType))
  insertType sym tType
  return nRefMap

handleCycles :: MonadTran m => [Absyn.Dec] -> Absyn.Pos -> m ()
handleCycles decs pos = foldM (recurse Set.empty) varSet varSet >> return ()
  where
    varSet = foldr f mempty decs
    f (Absyn.TypeDec s (Absyn.NameTy {}) _) set = Set.insert s set
    f _                                     set = set
    recurse seen set x
      | Set.member x seen = throwError (show pos <> " cycle detected " <> show seen <> " is in a cycle")
      | otherwise         = do Trans {tm} <- get
                               case Map.lookup x tm of
                                 Just (PT.NAME v ref) | Set.member v set -> do
                                       mRef <- liftIO (Ref.readIORef ref)
                                       recurse (Set.insert x seen) set v
                                 _ -> return (set Set.\\ seen)

insertType :: MonadTran m => S.Symbol -> PT.Type -> m ()
insertType sym tp = do
  trans@(Trans {tm = typeMap}) <- get
  put (trans {tm = Map.insert sym tp typeMap})

-- Helper functions----------------------------------------------------------------------------

mapFieldDec f (Absyn.FieldDec nameSym typSym pos) = do
  Trans {tm} <- get
  case tm Map.!? typSym of
    Just typ -> return (f nameSym typ)
    Nothing  -> throwError (show pos <> " var " <> S.unintern nameSym
                                     <> " can't be typed " <> S.unintern typSym
                                     <> " is undefined" )

getOrCreateRefMap :: MonadTran m => RefMap -> S.Symbol -> m (Ref.IORef (Maybe PT.Type), RefMap)
getOrCreateRefMap refMap sym =
  case refMap Map.!? sym of
    Nothing -> do
      Trans {tm = typeMap} <- get
      newRef <- liftIO (Ref.newIORef (typeMap Map.!? sym))
      return (newRef, Map.insert sym newRef refMap)
    Just ref -> return (ref, refMap)

-- probably not actually needed, if we don't mutate types at all... is only really relevant
-- when we start interpreting code... so if that doesn't happen at this pass, replace with
-- adds a symbol to the envEntry replacing what is there for this scope
locallyInsert1 :: MonadTran m => m b -> (S.Symbol, Env.Entry) -> m b
locallyInsert1 e x = locallyInsert e [x]

locallyInsert :: MonadTran m => m b -> [(S.Symbol, Env.Entry)] -> m b
locallyInsert expression xs = do
  Trans {tm = typeMap, em = envMap} <- get
  let vals = fmap (\(symb,_) -> (symb, envMap Map.!? symb)) xs

  traverse (uncurry changeEnvValue . fmap Just) xs
  expResult <- expression
  traverse (uncurry changeEnvValue) vals
  return expResult

-- Changes the Environment value... removing a value if there is none, else places the new value in the map
changeEnvValue :: MonadTran m => S.Symbol -> Maybe Env.Entry -> m ()
changeEnvValue symb val = do
  trans@(Trans {em = envMap}) <- get
  case val of
    Nothing   -> put (trans {em = Map.delete symb envMap})
    Just val' -> put (trans {em = Map.insert symb val' envMap})

-- this function will eventually become deprecated once we handle the intermediate stage
handleInfixExp :: MonadTran m
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
handleInfixSame :: MonadTran m => Bool -> Absyn.Exp -> Absyn.Exp -> Absyn.Pos -> m Expty
handleInfixSame inLoop left right pos = do
  left'  <- transExp' inLoop left
  right' <- transExp' inLoop right
  checkSame left' right' pos
  return (Expty {expr = (), typ = PT.INT})

handleInfixInt, handleInfixStrInt, handleInfixStr
  :: MonadTran m => Bool -> Absyn.Exp -> Absyn.Exp -> Absyn.Pos -> m Expty
handleInfixInt    = handleInfixExp checkInt
handleInfixStr    = handleInfixExp checkStr
handleInfixStrInt = handleInfixExp checkStrInt

checkArr :: (Show a, MonadTranErr m) => Expty -> a -> m PT.Type
checkArr (Expty {typ = PT.ARRAY typ _}) pos = return typ
checkArr (Expty {typ = _})              pos = throwError (show pos <> " Array type required")

checkInt, checkNil, checkStrInt :: (MonadTranErr m, Show a) => Expty -> a -> m ()

checkInt (Expty {typ = PT.INT}) pos = return  ()
checkInt (Expty {typ = _})      pos = throwError (show pos <> " integer required")

checkNil (Expty {typ = PT.NIL}) pos = return ()
checkNil (Expty {typ = _})      pos = throwError (show pos <> " null required")

checkStr (Expty {typ = PT.STRING}) pos = return  ()
checkStr (Expty {typ = _})         pos = throwError (show pos <> " string required")

checkSame :: (MonadTranErr m, Show a) => Expty -> Expty -> a -> m ()
checkSame (Expty {typ = x}) (Expty {typ = y}) = checkSameTyp x y

checkStrInt (Expty {typ = PT.STRING}) pos = return  ()
checkStrInt (Expty {typ = PT.INT})    pos = return  ()
checkStrInt (Expty {typ = _})         pos = throwError (show pos <> " integer or string required")

checkArrTyp (PT.ARRAY typ _) pos = return ()
checkArrTyp _                pos = throwError (show pos <> " Array type required")

checkRecType (PT.RECORD _ _) pos = return ()
checkRecType _               pos = throwError (show pos <> " record type required")

-- A variant of checkSame that works on PT.Type
checkSameTyp :: (MonadTranErr m, Show a) => PT.Type -> PT.Type -> a -> m ()
checkSameTyp (PT.RECORD _ xid) (PT.RECORD _ yid) pos
  | xid == yid = return ()
  | otherwise  = throwError (show pos <> " records are of different type")
checkSameTyp (PT.RECORD _ xid) PT.NIL pos = return ()
checkSameTyp PT.NIL (PT.RECORD _ xid) pos = return ()
checkSameTyp (PT.ARRAY _ xid) (PT.ARRAY _ yid) pos
  | xid == yid = return ()
  | otherwise  = throwError (show pos <> " arrays are of different type")
checkSameTyp x y pos
  |  x == y   = return ()
  | otherwise = throwError (show pos <> " given a " <> show x <> " needs to be the same type as " <> show y)

isTypeDec :: Absyn.Dec -> Bool
isTypeDec (Absyn.TypeDec {}) = True
isTypeDec _                  = False

isFunctionDec (Absyn.FunDec {}) = True
isFunctionDec _                 = False

isVarDec (Absyn.VarDec {}) = True
isVarDec _                 = False

actualType :: PT.Type -> IO PT.Type
actualType (PT.NAME sym ref) = do
  mtp <- Ref.readIORef ref
  case mtp of
    Just tp -> actualType tp
    Nothing -> return (PT.NAME sym ref)
actualType typ = return typ
