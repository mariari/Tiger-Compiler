{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module SemanticAnalysis where

import qualified ProgramTypes as PT
import qualified TigerType    as Absyn

import           Data.Unique
import qualified Data.Symbol as S
import qualified Data.Map.Strict as Map -- we are use ordering in symbols, so we can't use HashMap
import           Control.Monad.State.Lazy

-- Environment
type TypeMap = PT.SymMap PT.Type  -- for types
type EnvMap  = PT.SymMap EnvEntry -- for functions and variables

data Translation = Trans { tm :: TypeMap
                         , em :: EnvMap
                         } deriving Show

type MonadTranslation m    = MonadState Translation m
type MonadTranlsationImp m = (MonadIO m, MonadTranslation m)

data EnvEntry = VarEntry {ty :: PT.Type}
              | FunEntry {formals :: [PT.Type]
                         ,result  :: PT.Type}
              deriving Show

baseTenv :: TypeMap
baseTenv = Map.fromList [(S.intern "int",    PT.INT)
                        ,(S.intern "string", PT.STRING)
                        ,(S.intern "nil",    PT.NIL)]

baseVenv :: EnvMap
baseVenv = Map.fromList [(S.intern "print",     FunEntry [PT.STRING] PT.UNIT)
                        ,(S.intern "flush",     FunEntry []          PT.UNIT)
                        ,(S.intern "getchar",   FunEntry []          PT.STRING)
                        ,(S.intern "ord",       FunEntry [PT.STRING] PT.INT)
                        ,(S.intern "chr",       FunEntry [PT.INT]    PT.STRING)
                        ,(S.intern "size",      FunEntry [PT.STRING] PT.INT)
                        ,(S.intern "not",       FunEntry [PT.INT]    PT.INT)
                        ,(S.intern "exit",      FunEntry [PT.INT]    PT.UNIT)
                        ,(S.intern "substring", FunEntry [PT.STRING, PT.INT, PT.INT] PT.STRING)
                        ,(S.intern "concat",    FunEntry [PT.STRING, PT.STRING]      PT.STRING)]


-- Translation type
-- will be expanded upon in chapter 7
type TranslateExp = ()

data Expty = Expty { exp :: TranslateExp
                   , typ :: PT.Type
                   } deriving Show

transVar :: MonadTranslation m => Absyn.Var -> m TranslateExp
transVar = undefined

transExp :: MonadTranslation m => Absyn.Exp -> m TranslateExp
transExp = undefined