{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Semantic.Analysis where

import App.Environment
import App.Initialize
import qualified ProgramTypes         as PT
import qualified AbstractSyntax       as Absyn
import qualified Semantic.Environment as Env
import qualified Semantic.Translate   as T
import qualified Semantic.Temp        as Temp
import qualified Semantic.Fragment    as F

import           Control.Lens
import           Data.Maybe
import qualified Data.IORef      as Ref
import qualified Data.List       as List
import qualified Data.Symbol     as S
import qualified Data.Map.Strict as Map -- we are use ordering in symbols, so we can't use HashMap
import qualified Data.Set        as Set
import           Control.Monad.State.Lazy
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Identity
import           Data.Foldable (traverse_)

type RefMap = Map.Map S.Symbol (Ref.IORef (Maybe PT.Type))

data Expty = Expty { _expr :: !T.Exp
                   , _typ  :: !PT.Type
                   } deriving Show
makeLenses ''Expty

data VarTy = VarTy { _var  :: !Env.Entry
                   , _expr :: !T.Exp
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

data ExtraData = Ex { _inLoop :: Maybe Temp.Label
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
type MonadTran    m = ( MonadState Translation m
                      , MonadTranErr m
                      , MonadIO m
                      , MonadReader Env m) -- IO is for refs only

runMonadTran :: Env.TypeMap
             -> Env.EntryMap
             -> Env
             -> (StateT Translation (ExceptT e (ReaderT Env IO))) a
             -> IO (Either e (a, Translation))
runMonadTran tm em env f = runReaderT (runExceptT (runStateT f trans)) env
  where trans = Trans {_tm = tm, _em = em, _uniq = 0}

transExp :: Env.TypeMap -> Env.EntryMap -> Absyn.ExpI -> Env -> IO [F.Frag]
transExp tm em absyn env = do
  mainLevel <- T.mainLevel
  x <- runMonadTran tm em env (transExp' (Ex {_inLoop = Nothing, _level = mainLevel}) absyn)
  case x of
    Left a          -> error a
    Right (expt,tl) -> do
      runExceptT (runReaderT (T.procEntryExit mainLevel (expt^.expr)) env)
      Ref.readIORef (env^.frag)

transExp' :: MonadTran m => ExtraData -> Absyn.ExpI -> m Expty
transExp' _ (Absyn.IntLit x _)    = return (Expty {_expr = T.intLit x, _typ = PT.INT})
transExp' _ (Absyn.Nil _)         = return (Expty {_expr = T.nil, _typ = PT.NIL})
transExp' _ (Absyn.StringLit s _) = (\s -> Expty {_expr = s, _typ = PT.STRING}) <$> T.stringLit s
transExp' e (Absyn.Var x)         = varTytoExpTy <$> transVar e x

transExp' exData (Absyn.Infix' left x right pos) =
  let f = case x of
            Absyn.Minus -> handleInfixInt
            Absyn.Plus  -> handleInfixInt
            Absyn.Times -> handleInfixInt
            Absyn.Div   -> handleInfixInt
            Absyn.And   -> handleInfixInt
            Absyn.Or    -> handleInfixInt
            Absyn.Gt    -> handleInfixStrInt
            Absyn.Ge    -> handleInfixStrInt
            Absyn.Lt    -> handleInfixStrInt
            Absyn.Le    -> handleInfixStrInt
            Absyn.Eq    -> handleInfixSame
            Absyn.Neq   -> handleInfixSame
  in f exData left x right pos

transExp' exData (Absyn.Negation val pos) = do
  val' <- transExp' exData val
  checkInt val' pos
  return (over expr id val') -- over for future transactions

transExp' exData (Absyn.Sequence [] pos) =
  liftIO $ (\_expr -> Expty {_expr, _typ = PT.NIL}) <$> (T.sequence [])
transExp' exData (Absyn.Sequence xs pos) = do
  exs <- traverse (transExp' exData) xs
  exp <- liftIO (T.sequence (view expr <$> exs))
  return $ set expr exp (last exs)

transExp' (Ex {_inLoop = Just x}) (Absyn.Break pos) =
  return (Expty {_expr = T.break x, _typ = PT.NIL})
transExp' _ (Absyn.Break pos) =
  throwError (show pos <> " break needs to be used inside a loop")

transExp' exData (Absyn.While pred body pos) = do
  newBreak <- liftIO (Temp.newLabel)
  pred'    <- transExp' exData pred
  body'    <- transExp' (set inLoop (Just newBreak) exData) body
  checkInt     pred' pos
  checkNilUnit body' pos
  return (over expr id body')

transExp' exData (Absyn.For var esc from to body pos) = do
  newBreak <- liftIO (Temp.newLabel)
  from' <- transExp' exData from
  to'   <- transExp' exData to
  checkInt from' pos
  checkInt to' pos
  (exData, access) <- allocLocal exData esc
  body' <- locallyInsert1 (transExp' (set inLoop (Just newBreak) exData) body)
                          (var, Env.VarEntry {_ty = PT.INT, _modifiable = False, _access = access})
  checkNilUnit body' pos -- the false makes it so if we try to modify it, it errors
  return (over expr id body')

transExp' exData (Absyn.IfThenElse pred then' else' pos) = do
  pred'  <- transExp' exData pred
  then'' <- transExp' exData then'
  else'' <- transExp' exData else'
  checkInt pred' pos
  checkSame then'' else'' pos
  return (over expr id then'')

transExp' exData (Absyn.IfThen pred then' pos) = do
  pred'  <- transExp' exData pred
  then'' <- transExp' exData then'
  checkInt pred' pos
  checkNilUnit then'' pos
  return (over expr id then'')

transExp' exData (Absyn.Funcall fnSym args pos) = do
  trans <- get
  case trans^.em.at fnSym of
    Nothing                -> throwError (show pos <> " function " <> S.unintern fnSym <> " is not defined")
    Just (Env.VarEntry {}) -> throwError (show pos <> " variable " <> S.unintern fnSym <> " is not a function")
    Just (Env.FunEntry {_formals, _result, _level, _label}) -> do
      expArgs <- zipWithM (\arg formal -> do
                             Expty {_typ, _expr} <- transExp' exData arg
                             actualFormal <- liftIO (actualType formal)
                             actualArg    <- liftIO (actualType _typ)
                             checkSameTyp actualFormal actualArg pos
                             return _expr)
                args
                _formals
      _expr <- T.funcall fnSym _label _level expArgs (exData^.level)
      return (Expty {_expr, _typ = _result})

transExp' exData (Absyn.Assign var toPutE pos) = do
  VarTy {_var = envVar, _expr} <- transVar exData var
  toPut                        <- transExp' exData toPutE
  case envVar of
    Env.FunEntry {}                    -> throwError (show pos <> " can't set a function")
    Env.VarEntry {_modifiable = False} -> throwError (show pos <> " variable is not modifiable")
    Env.VarEntry {_ty = varType}       -> do
      checkSameTyp varType (toPut^.typ) pos
      _expr <- liftIO (T.assign _expr (toPut^.expr))
      return $ Expty {_expr, _typ = PT.UNIT}

transExp' exData (Absyn.ArrCreate tyid length content pos) = do
  Trans {_tm = typeMap} <- get
  lengthExp             <- transExp' exData length
  checkInt lengthExp pos
  arrType <- liftIO (traverse actualType (typeMap Map.!? tyid))
  case arrType of
    Just (PT.ARRAY arrTyp uniqueId) -> do
      content <- transExp' exData content
      _expr   <- liftIO (T.arrCreate (view expr lengthExp) (view expr content))
      checkSameTyp arrTyp (content^.typ) pos
      return (Expty {_expr, _typ = (PT.ARRAY arrTyp uniqueId)})
    Nothing -> throwError (show pos <> " array type " <> S.unintern tyid <> " undefined")
    Just x  -> throwError (show pos <> " " <> S.unintern tyid
                                    <> " is not of type array, but of type " <> show x)

transExp' exData (Absyn.RecCreate tyid givens pos) = do
  Trans {_tm = typeMap} <- get
  recType               <- liftIO $ traverse actualType (typeMap Map.!? tyid)
  case recType of
    Just (PT.RECORD syms uniqueType) -> do
      let sortedRecType = List.sortOn fst syms              -- with these two sorted, we can just compare
          sortedGivens  = List.sortOn Absyn.fieldTyp givens -- and error from there
      sortedGivenTypes <- traverse (\ (Absyn.Field sym exp pos) -> do
                                       e <- transExp' exData exp
                                       return (sym, e, pos))
                                   sortedGivens
      zipWithM_ (\ (_, recType)
                   (_, givenType, p) -> do
                    recTyp <- liftIO (actualType recType)
                    gvnTyp <- liftIO (actualType (view typ givenType))
                    checkSameTyp recTyp gvnTyp p)
                sortedRecType
                sortedGivenTypes
      _expr <- liftIO $ T.recCreate (view (_2 . expr) <$> sortedGivenTypes)
      return (Expty {_expr = _expr
                    , _typ = PT.RECORD syms uniqueType
                    })
    Nothing -> throwError (show pos <> " record " <> S.unintern tyid <> " undefined")
    Just x  -> throwError (show pos <> " " <> S.unintern tyid
                                    <> " is not of type record, but of type " <> show x)

transExp' exData (Absyn.Let decs exps pos) = do
  currentEnv <- get
  expDecs    <- transDec exData decs pos
  case exps of
    []   -> liftIO $ (\_expr -> Expty {_expr, _typ = PT.NIL}) <$> T.letExp expDecs []
    exps -> do
      expsTyped <- traverse (transExp' exData) exps
      put currentEnv
      _expr <- liftIO (T.letExp expDecs (fmap (view expr) expsTyped))
      return $ set expr _expr (last expsTyped)

transVar :: MonadTran m => ExtraData -> Absyn.Var Identity -> m VarTy
transVar exData (Absyn.SimpleVar sym pos) = do
  let err str = throwError (show pos <> " " <> S.unintern sym <> str)
  Trans {_em = envMap} <- get
  case envMap Map.!? sym of
    Nothing                  -> err " is not defined"
    Just v@(Env.FunEntry {}) -> err " is a function"
    Just v@(Env.VarEntry {_ty, _access}) -> do
      actualTy <- liftIO (actualType _ty)
      _expr    <- T.simpleVar _access (exData^.level)
      return $ VarTy { _var  = set Env.ty actualTy v
                     , _expr
                     }

transVar exData (Absyn.Subscript arrayType expInt pos) = do
  intExpty <- transExp' exData expInt -- not going to allow breaking in an array lookup!
  checkInt intExpty pos
  VarTy {_var, _expr} <- transVar exData arrayType
  case _var of
    Env.FunEntry {} -> throwError (show pos <> " tried to do array lookup on a function")
    v@(Env.VarEntry {_ty}) -> do
      actualTy <- liftIO (actualType _ty)
      ty       <- checkArrTyp actualTy pos
      aTy      <- liftIO (actualType ty)
      return (VarTy { _expr, _var = set Env.ty aTy v })

transVar exData (Absyn.FieldVar recordType field pos) = do
  VarTy {_var, _expr} <- transVar exData recordType
  case _var of
    Env.FunEntry {} -> throwError (show pos <> " tried to do record lookup on a function")
    v@Env.VarEntry {_ty} -> do
      actualTy <- liftIO (actualType _ty)
      lst      <- checkRecType actualTy pos
      case List.find (\(sym,_) -> sym == field) lst of
        Just (_, ty) -> do
          aTy <- liftIO (actualType ty)
          return (VarTy { _expr, _var = set Env.ty aTy v })
        Nothing ->
          throwError (show pos <> " Field in " <> S.unintern field <> "Record does not exist")

transTy :: MonadTran m => RefMap -> Absyn.Ty Identity -> m (PT.Type, RefMap)
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

transDec :: MonadTran m => ExtraData -> [Absyn.Dec Identity] -> Absyn.Pos -> m [T.Exp]
transDec exData decs pos = do
  _  <- transTypeDecs     exData typeDecs pos
  _  <- transFunDecsHead  exData funDecs  pos
  t3 <- transVarDecs      exData varDecs  pos
  _  <- transFunDecsBody  exData funDecs  pos
  return t3
  where
    typeDecs = filter isTypeDec     decs
    funDecs  = filter isFunctionDec decs
    varDecs  = filter isVarDec      decs

transVarDecs exData decs pos = traverse f decs
  where
    f (Absyn.VarDec sym escRef mType exp _) = do
      Expty {_typ, _expr} <- transExp' exData exp
      trans               <- get
      (exData, access)    <- allocLocal exData escRef
      let newMap = do
            modify
              . over em
              . Map.insert sym
              $ Env.VarEntry { _ty         = _typ
                             , _modifiable = True
                             , _access     = access
                             }
            simVar <- T.simpleVar access (exData^.level)
            liftIO $ T.assign simVar _expr
      case mType of
        Nothing  -> newMap
        Just sty ->
          case trans^.tm.at sty of
            Nothing -> throwError (show pos <> " type " <> S.unintern sty <> " is not defined ")
            Just x -> do
              checkSameTyp x _typ pos
              newMap
    f _ = throwError (show pos <> " violated precondition ")

transFunDecsHead :: (MonadTran m, Traversable t, Show a) => ExtraData -> t (Absyn.Dec Identity) -> a -> m ()
transFunDecsHead exData decs pos = traverse_ f decs
  where
    f (Absyn.FunDec name fields mtype body pos) = do
      trans <- get
      types <- traverse (mapFieldDec (\_ x _ -> x)) fields
      let putIn x = do
            label   <- liftIO Temp.newLabel
            let escapes = map (\(Absyn.FieldDec _ (Identity esc) _ _) -> esc) fields
            lvl     <- liftIO $ T.newLevel (exData^.level) label escapes
            modify (over em (Map.insert name (Env.FunEntry types x lvl label)))
      case mtype of
        Nothing      -> putIn PT.UNIT
        Just symType ->
          case trans^.tm.at symType of
            Nothing -> throwError (show pos <> " type " <> S.unintern symType <> " is undeifned")
            Just x  -> do
              actualTy <- liftIO (actualType x)
              putIn actualTy
    f _ = throwError (show pos <> " internal precondition violated at transFunDecHead")

transFunDecsBody :: (MonadTran m, Traversable t, Show a) => ExtraData -> t (Absyn.Dec Identity) -> a -> m ()
transFunDecsBody exData decs pos = traverse_ f decs
  where
    f (Absyn.FunDec name fields mtype body pos) = do
      trans <- get
      case trans^.em.at name of
        Just (Env.FunEntry {_result, _level, _label}) -> do
          (exData, types)    <- allocFields exData fields
          Expty {_typ,_expr} <- locallyInsert (transExp' exData body) types
          bodyType           <- liftIO (actualType _typ)
          trueResultType     <- liftIO (actualType _result)
          checkSameTyp trueResultType bodyType pos
          T.functionDec _level _expr
        _ -> throwError (show pos <> " transFunDecsHead did not put the function in the map")
    f _ = throwError (show pos <> " internal precondition violated at transFunDecBody")

makeVar exData n t escRef = do
  (exData, access) <- allocLocal exData escRef
  return (exData, n, Env.VarEntry { _ty = t, _modifiable = True, _access = access})

allocFields exData fields = do
  (newExData, vars) <- foldM f (exData,[]) fields
  return (newExData, reverse vars)
  where
    f (exData, vars) field = do
      (exData', name, var) <- mapFieldDecImp (makeVar exData) field
      return (exData', (name, var) : vars)

transTypeDecs :: MonadTran m => ExtraData -> [Absyn.Dec Identity] -> Absyn.Pos -> m ()
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

writeWithRef :: MonadTran m => Ref.IORef (Maybe PT.Type) -> S.Symbol -> Absyn.Ty Identity -> RefMap -> m RefMap
writeWithRef symRef sym ty refMap = do
  (tType, nRefMap) <- transTy (Map.insert sym symRef refMap) ty
  liftIO (Ref.writeIORef symRef (Just tType))
  insertType sym tType
  return nRefMap

handleCycles :: MonadTran m => [Absyn.Dec Identity] -> Absyn.Pos -> m ()
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
insertType sym tp = modify (over tm (Map.insert sym tp))

-- Helper functions----------------------------------------------------------------------------

allocLocal :: (MonadIO m, MonadError String m) => ExtraData -> Identity Bool -> m (ExtraData, T.Access)
allocLocal (Ex inLoop level) escI = do
  let Identity esc = escI
  (level', access)<- T.allocLocal level esc
  return (Ex inLoop level', access)

mapFieldDecImp f (Absyn.FieldDec nameSym esc typSym pos) = do
  trans <- get
  case trans^.tm^.at typSym of
    Just typ -> f nameSym typ esc
    Nothing  -> throwError (show pos <> " var " <> S.unintern nameSym
                                     <> " can't be typed " <> S.unintern typSym
                                     <> " is undefined" )

mapFieldDec f = mapFieldDecImp (\x y z -> return (f x y z))

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
changeEnvValue symb Nothing    = modify (over em (Map.delete symb))
changeEnvValue symb (Just val) = modify (over em (Map.insert symb val))

-- this function will eventually become deprecated once we handle the intermediate stage
handleInfixExp :: MonadTran m
               => (Expty -> Absyn.Pos -> m ()) -- A function like checkInt
               -> ExtraData                    -- loop and level information
               -> Absyn.ExpI                   -- left side of infix
               -> Absyn.Op                     -- operation
               -> Absyn.ExpI                   -- right side of infix
               -> Absyn.Pos                    -- the Posiiton
               -> m Expty
handleInfixExp f inLoop left op right pos = do
  left'  <- transExp' inLoop left
  right' <- transExp' inLoop right
  f left' pos
  f right' pos
  _expr <- liftIO (T.infix' (left'^.expr) op (right'^.expr))
  return (Expty {_expr, _typ = PT.INT})

-- will become deprecated once we handle the intermediate stage
handleInfixSame :: MonadTran m => ExtraData -> Absyn.ExpI -> Absyn.Op -> Absyn.ExpI -> Absyn.Pos -> m Expty
handleInfixSame inLoop left op right pos = do
  left'  <- transExp' inLoop left
  right' <- transExp' inLoop right
  checkSame left' right' pos
  _expr <- liftIO (T.infix' (left'^.expr) op (right'^.expr))
  return (Expty {_expr, _typ = PT.INT})

handleInfixInt, handleInfixStrInt, handleInfixStr
  :: MonadTran m => ExtraData -> Absyn.ExpI -> Absyn.Op -> Absyn.ExpI -> Absyn.Pos -> m Expty
handleInfixInt    = handleInfixExp checkInt
handleInfixStr    = handleInfixExp checkStr
handleInfixStrInt = handleInfixExp checkStrInt

checkArr :: (Show a, MonadTranErr m) => Expty -> a -> m PT.Type
checkArr (Expty {_typ = PT.ARRAY typ _}) pos = return typ
checkArr (Expty {})                      pos = throwError (show pos <> " Array type required")

checkInt, checkNil, checkStrInt :: (MonadTranErr m, Show a) => Expty -> a -> m ()

checkInt (Expty {_typ = PT.INT}) pos = return  ()
checkInt (Expty {})              pos = throwError (show pos <> " integer required")

checkNilUnit (Expty {_typ = PT.UNIT}) pos = return ()
checkNilUnit (Expty {_typ = PT.NIL})  pos = return ()
checkNilUnit (Expty {_typ = _})       pos = throwError (show pos <> " nil or unit required")

checkNil (Expty {_typ = PT.NIL}) pos = return ()
checkNil (Expty {_typ = _})      pos = throwError (show pos <> " nil required")

checkStr (Expty {_typ = PT.STRING}) pos = return  ()
checkStr (Expty {_typ = _})         pos = throwError (show pos <> " string required")

checkSame :: (MonadTranErr m, Show a) => Expty -> Expty -> a -> m ()
checkSame (Expty {_typ = x}) (Expty {_typ = y}) = checkSameTyp x y

checkStrInt (Expty {_typ = PT.STRING}) pos = return  ()
checkStrInt (Expty {_typ = PT.INT})    pos = return  ()
checkStrInt (Expty {_typ = _})         pos = throwError (show pos <> " integer or string required")

checkArrTyp (PT.ARRAY typ _) pos = return typ
checkArrTyp _                pos = throwError (show pos <> " Array type required")

checkRecType (PT.RECORD xs _) pos = return xs
checkRecType _                pos = throwError (show pos <> " record type required")

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

isTypeDec :: Absyn.Dec Identity -> Bool
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
