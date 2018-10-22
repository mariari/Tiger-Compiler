module Semantic.Escape where

import qualified AbstractSyntax as Abs

import qualified Data.IORef  as R
import qualified Data.Map    as Map
import qualified Data.Symbol as S
import Data.Foldable (traverse_)
import Control.Monad (foldM)

type Depth = Int

type EscEnvMap = Map.Map S.Symbol (Depth, R.IORef Bool)

emptyMap = (mempty,0)

traverseExp :: (EscEnvMap, Depth) -> Abs.Exp -> IO ()
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

traverseVar :: (EscEnvMap, Depth) -> Abs.Var -> IO ()
traverseVar md (Abs.FieldVar v _ _)    = traverseVar md v
traverseVar md (Abs.Subscript v exp _) = traverseVar md v >> traverseExp md exp
traverseVar (env,depth) (Abs.SimpleVar sym _) =
  case env Map.!? sym of
    Just (vDepth, esc) | depth > vDepth -> R.writeIORef esc True
    _                                   -> return ()

traverseDec :: (EscEnvMap, Depth) -> Abs.Dec -> IO (EscEnvMap,Depth)
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
