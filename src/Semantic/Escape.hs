module Semantic.Escape (escape) where

import qualified AbstractSyntax as Abs

import qualified Data.IORef  as R
import qualified Data.Map    as Map
import qualified Data.Symbol as S
import           Control.Monad.Identity
import           Data.Foldable (traverse_)
import           Control.Monad (foldM)

type Depth = Int

type EscEnvMap = Map.Map S.Symbol (Depth, R.IORef Bool)

escape :: Abs.ExpI -> IO Abs.ExpI
escape exp = do
  ref <- expRefOfexpI exp
  traverseExp (mempty,0) ref
  expIOexpRef ref

expRefOfexpI :: Abs.ExpI -> IO (Abs.Exp R.IORef)
expRefOfexpI = expConvertGen f
  where
    f (Identity esc) = R.newIORef esc

expIOexpRef :: Abs.Exp R.IORef -> IO (Abs.ExpI)
expIOexpRef = expConvertGen f
  where
    f = fmap Identity . R.readIORef

traverseExp :: (EscEnvMap, Depth) -> Abs.Exp R.IORef -> IO ()
traverseExp _  (Abs.Nil       {})        = return ()
traverseExp _  (Abs.IntLit    {})        = return ()
traverseExp _  (Abs.StringLit {})        = return ()
traverseExp _  (Abs.Break     {})        = return ()
traverseExp md (Abs.Var var)             = traverseVar md var
traverseExp md (Abs.Negation exp _)      = traverseExp md exp
traverseExp md (Abs.Sequence xs _)       = traverse_ (traverseExp md) xs
traverseExp md (Abs.Funcall _ args _)    = traverse_ (traverseExp md) args
traverseExp md (Abs.RecCreate _ fs _)    = traverse_ (traverseExp md . Abs.expr) fs
traverseExp md (Abs.Infix' l  _ r _)     = traverseExp md l   >> traverseExp md r
traverseExp md (Abs.ArrCreate _ l ct _)  = traverseExp md l   >> traverseExp md ct
traverseExp md (Abs.Assign var exp _)    = traverseVar md var >> traverseExp md exp
traverseExp md (Abs.While prd bod _)     = traverseExp md prd >> traverseExp md bod
traverseExp md (Abs.IfThen p t    _)     = traverseExp md p   >> traverseExp md t
traverseExp md (Abs.IfThenElse p t e  _) = traverseExp md p   >> traverseExp md t >> traverseExp md e

traverseExp md (Abs.Let decs exps _) = do
  newMd <- foldM traverseDec md decs
  traverse_ (traverseExp newMd) exps

traverseExp (m,d) (Abs.For sym esc from to body _) = do
  let md = (Map.insert sym (d,esc) m, d)
  traverseExp md from
  traverseExp md to
  traverseExp md body

traverseVar :: (EscEnvMap, Depth) -> Abs.Var R.IORef -> IO ()
traverseVar md (Abs.FieldVar v _ _)    = traverseVar md v
traverseVar md (Abs.Subscript v exp _) = traverseVar md v >> traverseExp md exp
traverseVar (env,depth) (Abs.SimpleVar sym _) =
  case env Map.!? sym of
    Just (vDepth, esc) | depth > vDepth -> R.writeIORef esc True
    _                                   -> return ()

traverseDec :: (EscEnvMap, Depth) -> Abs.Dec R.IORef -> IO (EscEnvMap,Depth)
traverseDec md (Abs.TypeDec {}) = return md

traverseDec (env,d) (Abs.VarDec sym esc _ exp _) = do
  R.writeIORef esc False
  traverseExp (env,d) exp
  return (Map.insert sym (d,esc) env ,d)

-- functions can't escape!
traverseDec (env,d) (Abs.FunDec _ fields _ body _) = do
  let newDepth = d + 1
  let bodyEnv  = foldr (\(Abs.FieldDec sym esc _ _) -> Map.insert sym (newDepth, esc)) env fields
  traverseExp (bodyEnv,newDepth) body
  return (env,d)

-- a generic traversal to convert the Identity ref into IO-------------------------------------------

expConvertGen :: (Monad f, Show (esc1 Abs.Escape))
              => (esc2 Abs.Escape -> f (esc1 Abs.Escape))
              -> Abs.Exp esc2
              -> f (Abs.Exp esc1)
expConvertGen _ (Abs.Nil p)               = pure $ Abs.Nil p
expConvertGen _ (Abs.IntLit i p)          = pure $ Abs.IntLit i p
expConvertGen _ (Abs.StringLit s p)       = pure $ Abs.StringLit s p
expConvertGen _ (Abs.Break p)             = pure $ Abs.Break p
expConvertGen f (Abs.Var var)             = Abs.Var <$> varConvertGen f var
expConvertGen f (Abs.Negation e p)        = Abs.Negation <$> expConvertGen f e
                                                         <*> pure p
expConvertGen f (Abs.Sequence es p)       = Abs.Sequence <$> traverse (expConvertGen f) es
                                                         <*> pure p
expConvertGen f (Abs.ArrCreate s e1 e2 p) = Abs.ArrCreate s <$> expConvertGen f e1
                                                            <*> expConvertGen f e2
                                                            <*> pure p
expConvertGen f (Abs.RecCreate s fs p)    = Abs.RecCreate s <$> traverse (fieldConvertGen f) fs
                                                            <*> pure p
expConvertGen f (Abs.Infix' e1 op e2 p)   = Abs.Infix' <$> expConvertGen f e1
                                                       <*> pure op
                                                       <*> expConvertGen f e2
                                                       <*> pure p
expConvertGen f (Abs.Funcall s es p)      = Abs.Funcall s <$> traverse (expConvertGen f) es
                                                          <*> pure p
expConvertGen f (Abs.Assign v e p)        = Abs.Assign <$> varConvertGen f v
                                                       <*> expConvertGen f e
                                                       <*> pure p
expConvertGen f (Abs.IfThenElse pr t e p) = Abs.IfThenElse <$> expConvertGen f pr
                                                           <*> expConvertGen f t
                                                           <*> expConvertGen f e
                                                           <*> pure p
expConvertGen f (Abs.IfThen pr t p)       = Abs.IfThen <$> expConvertGen f pr
                                                       <*> expConvertGen f t
                                                       <*> pure p
expConvertGen f (Abs.While pr e p)       = Abs.While <$> expConvertGen f pr
                                                     <*> expConvertGen f e
                                                     <*> pure p
expConvertGen f (Abs.Let decs exps p)    = Abs.Let <$> traverse (decConvertGen f) decs
                                                   <*> traverse (expConvertGen f) exps
                                                   <*> pure p
expConvertGen f (Abs.For s esc e1 e2 e3 p) = Abs.For s <$> f esc
                                                       <*> expConvertGen f e1
                                                       <*> expConvertGen f e2
                                                       <*> expConvertGen f e2
                                                       <*> pure p

--varConvertGen :: Applicative f => (Abs.Exp esc -> f (Abs.Exp esc')) -> Abs.Var esc -> f (Abs.Var esc')
varConvertGen f (Abs.SimpleVar s p)     = pure (Abs.SimpleVar s p)
varConvertGen f (Abs.FieldVar esc s p)  = (\e -> Abs.FieldVar e s p) <$> varConvertGen f esc
varConvertGen f (Abs.Subscript v esc p) = Abs.Subscript <$> varConvertGen f v
                                                        <*> expConvertGen f esc
                                                        <*> pure p

decConvertGen f (Abs.FunDec s escps msym exp p) =
  Abs.FunDec s <$> traverse (fieldDecConvertGen f) escps
               <*> pure msym
               <*> expConvertGen f exp
               <*> pure p
decConvertGen f (Abs.VarDec s esc msym exp p) =
  Abs.VarDec s <$> f esc
               <*> pure msym
               <*> expConvertGen f exp
               <*> pure p
decConvertGen f (Abs.TypeDec s ty p) =
  Abs.TypeDec s <$> tyConvertGen f ty <*> pure p

tyConvertGen f (Abs.NameTy s p)  = return $ Abs.NameTy s p
tyConvertGen f (Abs.ArrayTy s p) = return $ Abs.ArrayTy s p
tyConvertGen f (Abs.RecordTy es) = Abs.RecordTy <$> traverse (fieldDecConvertGen f) es

--fieldConvertGen :: Applicative f => (Abs.Exp esc -> f (Abs.Exp esc')) -> Abs.Field esc -> f (Abs.Field esc')
fieldConvertGen f (Abs.Field s expr p) = Abs.Field s <$> expConvertGen f expr <*> pure p

fieldDecConvertGen f (Abs.FieldDec s1 esc s2 p) =
  Abs.FieldDec s1 <$> f esc <*> pure s2 <*> pure p

