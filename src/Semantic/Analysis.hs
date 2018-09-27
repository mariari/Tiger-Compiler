{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Semantic.Analysis where

import qualified ProgramTypes         as PT
import qualified AbstractSyntax       as Absyn
import qualified Semantic.Environment as Env
import qualified Semantic.Translate   as T

import           Control.Lens
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

data Expty = Expty { _expr :: !TranslateExp
                   , _typ  :: !PT.Type
                   } deriving Show
makeLenses ''Expty

data VarTy = VarTy { _var  :: !Env.Entry
                   , _expr :: !TranslateExp
                   } deriving Show

varTytoExpTy (VarTy {_var, _expr}) =
  let _typ = case _var of
        Env.VarEntry {_ty = ty}     -> ty
        Env.FunEntry {_result = ty} -> ty
  in Expty {_expr, _typ}

data Translation = Trans { _tm   :: !Env.TypeMap
                         , _em   :: !Env.EntryMap
                         , _uniq :: !Int
                         }  deriving Show
makeLenses ''Translation

data ExtraData = Ex { _inLoop :: Bool
                    , _level  :: T.Level
                    } deriving Show
makeLenses ''ExtraData

-- grabs a unique value from the Translation state
fresh :: MonadState Translation m => m Int
fresh = do
  trans <- get
  put (over uniq succ trans)
  return (trans^.uniq)

type MonadTranErr m = (MonadError String m)
type MonadTran    m = (MonadState  Translation m, MonadTranErr m, MonadIO m) -- IO is for refs only

runMonadTran :: Env.TypeMap
             -> Env.EntryMap
             -> (StateT Translation (ExceptT e IO)) a
             -> IO (Either e (a, Translation))
runMonadTran tm em f = runExceptT (runStateT f trans)
  where trans = Trans {_tm = tm, _em = em, _uniq = 0}

transExp :: Env.TypeMap -> Env.EntryMap -> Absyn.Exp -> IO Expty
transExp tm em absyn = do
  x <- runMonadTran tm em (transExp' (Ex {_inLoop = False, _level = T.outerMost}) absyn)
  case x of
    Left a          -> error a
    Right (expt,tl) -> return expt

transExp' :: MonadTran m => ExtraData -> Absyn.Exp -> m Expty
transExp' _ (Absyn.IntLit _ _)    = return (Expty {_expr = (), _typ = PT.INT})
transExp' _ (Absyn.Nil _)         = return (Expty {_expr = (), _typ = PT.NIL})
transExp' _ (Absyn.StringLit _ _) = return (Expty {_expr = (), _typ = PT.STRING})
transExp' e (Absyn.Var x)         = varTytoExpTy <$> transVar e x

transExp' exData (Absyn.Infix' left x right pos) = case x of
  Absyn.Minus -> handleInfixInt exData left right pos
  Absyn.Plus  -> handleInfixInt exData left right pos
  Absyn.Times -> handleInfixInt exData left right pos
  Absyn.Div   -> handleInfixInt exData left right pos
  Absyn.And   -> handleInfixInt exData left right pos
  Absyn.Or    -> handleInfixInt exData left right pos
  Absyn.Gt    -> handleInfixStrInt exData left right pos
  Absyn.Ge    -> handleInfixStrInt exData left right pos
  Absyn.Lt    -> handleInfixStrInt exData left right pos
  Absyn.Le    -> handleInfixStrInt exData left right pos
  Absyn.Eq    -> handleInfixSame exData left right pos
  Absyn.Neq   -> handleInfixSame exData left right pos

transExp' exData (Absyn.Negation val pos) = do
  val' <- transExp' exData val
  checkInt val' pos
  return (val' & expr %~ id) -- %~  for future transactions

transExp' exData (Absyn.Sequence [] pos) = return (Expty {_expr = (), _typ = PT.NIL})
transExp' exData (Absyn.Sequence xs pos) = last <$> traverse (transExp' exData) xs

transExp' (Ex {_inLoop}) (Absyn.Break pos)
  | _inLoop    = return (Expty {_expr = (), _typ = PT.NIL})
  | otherwise = throwError (show pos <> " break needs to be used inside a loop")

transExp' exData (Absyn.While pred body pos) = do
  pred' <- transExp' exData pred
  body' <- transExp' (exData & inLoop .~ True) body
  checkInt pred' pos
  checkNil body' pos
  return (body' & expr %~ id)

transExp' exData (Absyn.For var esc from to body pos) = do
  from' <- transExp' exData from
  to'   <- transExp' exData to
  checkInt from' pos
  checkInt to' pos
  body' <- locallyInsert1 (transExp' (exData & inLoop .~ True) body) -- could replace with a get and put, we if we don't mutate
                          (var, Env.VarEntry {Env._ty = PT.INT, Env._modifiable = False, _access = undefined})
  checkNil body' pos -- the false makes it so if we try to modify it, it errors
  return (body' & expr %~ id)

transExp' exData (Absyn.IfThenElse pred then' else' pos) = do
  pred'  <- transExp' exData pred
  then'' <- transExp' exData then'
  else'' <- transExp' exData else'
  checkInt pred' pos
  checkSame then'' else'' pos
  return (then'' & expr %~ id)

transExp' exData (Absyn.IfThen pred then' pos) = do
  pred'  <- transExp' exData pred
  then'' <- transExp' exData then'
  checkInt pred' pos
  checkNil then'' pos
  return (then'' & expr %~ id)

transExp' exData (Absyn.Funcall fnSym args pos) = do
  trans <- get
  case trans^.em.at fnSym of
    Nothing                -> throwError (show pos <> " function " <> S.unintern fnSym <> " is not defined")
    Just (Env.VarEntry {}) -> throwError (show pos <> " variable " <> S.unintern fnSym <> " is not a function")
    Just (Env.FunEntry {_formals, _result}) -> do
      zipWithM_ (\arg formal -> do
                    Expty {_typ} <- transExp' exData arg
                    actualFormal <- liftIO (actualType formal)
                    actualArg    <- liftIO (actualType _typ)
                    checkSameTyp actualFormal actualArg pos)
                args
                _formals
      return (Expty {_expr = (), _typ = _result})

transExp' exData (Absyn.Assign var toPutE pos) = do
  VarTy {_var = envVar} <- transVar exData var
  toPut                 <- transExp' exData toPutE
  case envVar of
    Env.FunEntry {}                    -> throwError (show pos <> " can't set a function")
    Env.VarEntry {_modifiable = False} -> throwError (show pos <> " variable is not modifiable")
    Env.VarEntry {_ty = varType}       -> (toPut & expr %~ id)
                                          <$ checkSameTyp varType (toPut^.typ) pos
transExp' exData (Absyn.ArrCreate tyid length content pos) = do
  Trans {_tm = typeMap} <- get
  lengthExp             <- transExp' exData length
  checkInt lengthExp pos
  arrType <- liftIO (traverse actualType (typeMap Map.!? tyid))
  case arrType of
    Just (PT.ARRAY arrTyp uniqueId) -> do
      content <- transExp' exData content
      checkSameTyp arrTyp (content^.typ) pos
      return (Expty {_expr = (), _typ = (PT.ARRAY arrTyp uniqueId)})
    Nothing -> throwError (show pos <> " array type " <> S.unintern tyid <> " undefined")
    Just x  -> throwError (show pos <> " " <> S.unintern tyid
                                    <> " is not of type array, but of type " <> show x)

transExp' exData (Absyn.RecCreate tyid givens pos) = do
  Trans {_tm = typeMap} <- get
  recType              <- liftIO $ traverse actualType (typeMap Map.!? tyid)
  case recType of
    Just (PT.RECORD syms uniqueType) -> do
      let sortedRecType = List.sortOn fst syms              -- with these two sorted, we can just compare
          sortedGivens  = List.sortOn Absyn.fieldTyp givens -- and error from there
      sortedGivenTypes <- traverse (\ (Absyn.Field sym exp pos) -> do
                                       e <- transExp' exData exp
                                       return (sym, e^.typ, pos))
                                   sortedGivens
      zipWithM_ (\ (_, recType)
                   (_, givenType, p) -> checkSameTyp recType givenType p)
                sortedRecType
                sortedGivenTypes
      return (Expty {_expr = (), _typ = PT.RECORD syms uniqueType})
    Nothing -> throwError (show pos <> " record " <> S.unintern tyid <> " undefined")
    Just x  -> throwError (show pos <> " " <> S.unintern tyid
                                    <> " is not of type record, but of type " <> show x)

transExp' exData (Absyn.Let decs exps pos) = do
  currentEnv <- get
  transDec exData decs pos
  case exps of
    []   -> return (Expty {_expr = (), _typ = PT.NIL})
    exps -> do
      expsTyped <- traverse (transExp' exData) exps
      put currentEnv
      return (last expsTyped)

transVar :: MonadTran m => ExtraData -> Absyn.Var -> m VarTy
transVar exData (Absyn.SimpleVar sym pos) = do
  Trans {_em = envMap} <- get
  case envMap Map.!? sym of
    Nothing ->
      throwError (show pos <> " " <> S.unintern sym <> " is not defined")
    Just v@(Env.FunEntry {_formals, _result}) -> -- can't store a function like this... change later?
      throwError (show pos <> " " <> S.unintern sym <> " is a function")
    Just v@(Env.VarEntry {_ty}) -> do
      actualTy <- liftIO (actualType _ty)
      return $ VarTy { _var = v & Env.ty .~ actualTy
                     , _expr = ()
                     }

-- this is why we need to be in state and not reader
transVar exData (Absyn.Subscript arrayType expInt pos) = do
  intExpty <- transExp' exData expInt -- not going to allow breaking in an array lookup!
  checkInt intExpty pos
  VarTy {_var, _expr} <- transVar exData arrayType
  case _var of
    Env.FunEntry {} -> throwError (show pos <> " tried to do array lookup on a function")
    v@(Env.VarEntry {_ty}) -> do
      actualTy <- liftIO (actualType _ty)
      checkArrTyp actualTy pos
      return (VarTy { _expr, _var = v & Env.ty .~ actualTy })

transVar exData (Absyn.FieldVar recordType field pos) = do
  VarTy {_var, _expr} <- transVar exData recordType
  case _var of
    Env.FunEntry {} -> throwError (show pos <> " tried to do record lookup on a function")
    v@Env.VarEntry {_ty} -> do
      actualTy <- liftIO (actualType _ty)
      checkRecType actualTy pos
      return (VarTy { _expr, _var = v & Env.ty .~ actualTy })

transTy :: MonadTran m => RefMap -> Absyn.Ty -> m (PT.Type, RefMap)
transTy refMap (Absyn.NameTy sym pos) = do
  (refType, refMap) <- getOrCreateRefMap refMap sym
  return (PT.NAME sym refType, refMap)
transTy refMap (Absyn.ArrayTy sym pos) = do
  (refType, refMap) <- getOrCreateRefMap refMap sym
  mtyp      <- liftIO (Ref.readIORef refType)
  uniqueNum <- fresh
  case mtyp of
    Nothing  -> return ((PT.ARRAY (PT.NAME sym refType) uniqueNum), refMap)
    Just typ -> return (PT.ARRAY typ uniqueNum, refMap)
transTy refMap (Absyn.RecordTy recs) = do
  (xs,refMap) <- foldM (\ (xs, refMap) (Absyn.FieldDec name esc tySym pos) -> do
                          (refType, refMap) <- getOrCreateRefMap refMap tySym
                          refValue          <- liftIO (Ref.readIORef refType)
                          return ((name, fromMaybe (PT.NAME tySym refType) refValue) : xs, refMap))
                       ([], refMap) recs
  unique <- fresh
  return (PT.RECORD (reverse xs) unique, refMap)

transDec :: MonadTran m => ExtraData -> [Absyn.Dec] -> Absyn.Pos -> m ()
transDec exData decs pos = do
  transTypeDecs     exData typeDecs pos
  transFunDecsHead  exData funDecs  pos
  transVarDecs      exData varDecs  pos
  transFunDecsBody  exData funDecs  pos
  where
    typeDecs = filter isTypeDec     decs
    funDecs  = filter isFunctionDec decs
    varDecs  = filter isVarDec      decs

transVarDecs exData decs pos = traverse f decs
  where
    f (Absyn.VarDec sym esc mType exp _) = do
      Expty {_typ} <- transExp' exData exp
      trans        <- get
      let newMap = modify (em %~ Map.insert sym (Env.VarEntry {_ty = _typ, _modifiable = True, _access = undefined}))
      case mType of
        Nothing  -> newMap
        Just sty ->
          case trans^.tm.at sty of
            Nothing -> throwError (show pos <> " type " <> S.unintern sty <> " is not defined ")
            Just x
              | x == _typ  -> newMap
              | otherwise -> throwError (show pos <> " " <> show _typ <> " is not " <> show x)
    f _ = throwError (show pos <> " violated precondition ")

transFunDecsHead :: (MonadTran m, Traversable t, Show a) => ExtraData -> t Absyn.Dec -> a -> m ()
transFunDecsHead exData decs pos = traverse_ f decs
  where
    f (Absyn.FunDec name fields mtype body pos) = do
      trans <- get
      types <- traverse (mapFieldDec (\_ x -> x)) fields
      let putIn x = modify (em %~ Map.insert name (Env.FunEntry types x undefined))
      case mtype of
        Nothing      -> putIn PT.UNIT
        Just symType ->
          case trans^.tm.at symType of
            Nothing -> throwError (show pos <> " type " <> S.unintern symType <> " is undeifned")
            Just x  -> do
              actualTy <- liftIO (actualType x)
              putIn actualTy
    f _ = throwError (show pos <> " internal precondition violated at transFunDecHead")

transFunDecsBody :: (MonadTran m, Traversable t, Show a) => ExtraData -> t Absyn.Dec -> a -> m ()
transFunDecsBody exData decs pos = traverse_ f decs
  where
    makeVar n t = (n, Env.VarEntry { _ty = t, _modifiable = True, _access = undefined })
    f (Absyn.FunDec name fields mtype body pos) = do
      trans        <- get
      types        <- traverse (mapFieldDec makeVar) fields
      Expty {_typ} <- locallyInsert (transExp' exData body) types
      bodyType     <- liftIO (actualType _typ)
      case trans^.em.at name of
        Just (Env.FunEntry {_result}) -> do
          trueResultType <- liftIO (actualType _result)
          checkSameTyp trueResultType bodyType pos
        _ -> throwError (show pos <> " transFunDecsHead did not put the function in the map")
    f _ = throwError (show pos <> " internal precondition violated at transFunDecBody")

transTypeDecs :: MonadTran m => ExtraData -> [Absyn.Dec] -> Absyn.Pos -> m ()
transTypeDecs exData decs pos = foldM handle1 refMap decs >>= allDefined >> handleCycles decs pos
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
    handle1 refMap _  = throwError (show pos <> " a non-type was sent to transTypeDecs")
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
      | otherwise         = do
          trans <- get
          case Map.lookup x (trans^.tm) of
            Just (PT.NAME v _) | Set.member v set -> recurse (Set.insert x seen) set v
            _                                     -> return (set Set.\\ seen)

insertType :: MonadTran m => S.Symbol -> PT.Type -> m ()
insertType sym tp = modify (tm %~ Map.insert sym tp)

-- Helper functions----------------------------------------------------------------------------

mapFieldDec f (Absyn.FieldDec nameSym esc typSym pos) = do
  trans <- get
  case trans^.tm^.at typSym of
    Just typ -> return (f nameSym typ)
    Nothing  -> throwError (show pos <> " var " <> S.unintern nameSym
                                     <> " can't be typed " <> S.unintern typSym
                                     <> " is undefined" )

getOrCreateRefMap :: MonadTran m => RefMap -> S.Symbol -> m (Ref.IORef (Maybe PT.Type), RefMap)
getOrCreateRefMap refMap sym =
  case refMap Map.!? sym of
    Nothing -> do
      trans  <- get
      newRef <- liftIO (Ref.newIORef (trans^.tm.at sym))
      return (newRef, Map.insert sym newRef refMap)
    Just ref -> return (ref, refMap)

-- probably not actually needed, if we don't mutate types at all... is only really relevant
-- when we start interpreting code... so if that doesn't happen at this pass, replace with
-- adds a symbol to the envEntry replacing what is there for this scope
locallyInsert1 :: MonadTran m => m b -> (S.Symbol, Env.Entry) -> m b
locallyInsert1 e x = locallyInsert e [x]

locallyInsert :: MonadTran m => m b -> [(S.Symbol, Env.Entry)] -> m b
locallyInsert expression xs = do
  trans <- get
  let vals = fmap (\(symb,_) -> (symb, trans^.em.at symb)) xs
  traverse (uncurry changeEnvValue . fmap Just) xs
  expResult <- expression
  traverse (uncurry changeEnvValue) vals
  return expResult

-- Changes the Environment value... removing a value if there is none, else places the new value in the map
changeEnvValue :: MonadTran m => S.Symbol -> Maybe Env.Entry -> m ()
changeEnvValue symb Nothing    = modify (em %~ Map.delete symb)
changeEnvValue symb (Just val) = modify (em %~ Map.insert symb val)

-- this function will eventually become deprecated once we handle the intermediate stage
handleInfixExp :: MonadTran m
               => (Expty -> Absyn.Pos -> m ()) -- A function like checkInt
               -> ExtraData                    -- loop and level information
               -> Absyn.Exp                    -- left side of infix
               -> Absyn.Exp                    -- right side of infix
               -> Absyn.Pos                    -- the Posiiton
               -> m Expty
handleInfixExp f inLoop left right pos = do
  left'  <- transExp' inLoop left
  right' <- transExp' inLoop right
  f left' pos
  f right' pos
  return (Expty {_expr = (), _typ = PT.INT})

-- will become deprecated once we handle the intermediate stage
handleInfixSame :: MonadTran m => ExtraData -> Absyn.Exp -> Absyn.Exp -> Absyn.Pos -> m Expty
handleInfixSame inLoop left right pos = do
  left'  <- transExp' inLoop left
  right' <- transExp' inLoop right
  checkSame left' right' pos
  return (Expty {_expr = (), _typ = PT.INT})

handleInfixInt, handleInfixStrInt, handleInfixStr
  :: MonadTran m => ExtraData -> Absyn.Exp -> Absyn.Exp -> Absyn.Pos -> m Expty
handleInfixInt    = handleInfixExp checkInt
handleInfixStr    = handleInfixExp checkStr
handleInfixStrInt = handleInfixExp checkStrInt

checkArr :: (Show a, MonadTranErr m) => Expty -> a -> m PT.Type
checkArr (Expty {_typ = PT.ARRAY typ _}) pos = return typ
checkArr (Expty {})                      pos = throwError (show pos <> " Array type required")

checkInt, checkNil, checkStrInt :: (MonadTranErr m, Show a) => Expty -> a -> m ()

checkInt (Expty {_typ = PT.INT}) pos = return  ()
checkInt (Expty {})              pos = throwError (show pos <> " integer required")

checkNil (Expty {_typ = PT.NIL}) pos = return ()
checkNil (Expty {_typ = _})      pos = throwError (show pos <> " null required")

checkStr (Expty {_typ = PT.STRING}) pos = return  ()
checkStr (Expty {_typ = _})         pos = throwError (show pos <> " string required")

checkSame :: (MonadTranErr m, Show a) => Expty -> Expty -> a -> m ()
checkSame (Expty {_typ = x}) (Expty {_typ = y}) = checkSameTyp x y

checkStrInt (Expty {_typ = PT.STRING}) pos = return  ()
checkStrInt (Expty {_typ = PT.INT})    pos = return  ()
checkStrInt (Expty {_typ = _})         pos = throwError (show pos <> " integer or string required")

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
