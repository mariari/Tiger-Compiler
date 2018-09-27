{-# LANGUAGE TemplateHaskell #-}

module Semantic.Environment where

import           Frame.X86
import qualified Semantic.Translate as Trans
import qualified ProgramTypes       as PT
import qualified AbstractSyntax     as Absyn

import           Control.Lens
import qualified Data.Symbol     as S
import qualified Data.Map.Strict as Map -- we are use ordering in symbols, so we can't use HashMap

-- Environment
type TypeMap  = PT.SymMap PT.Type  -- for types
type EntryMap = PT.SymMap Entry -- for functions and variables

data Entry = VarEntry { _ty         :: !PT.Type
                      , _modifiable :: !Bool
                      , _access     :: Trans.Access
                      }
           | FunEntry { _formals :: ![PT.Type]
                      , _result  :: !PT.Type
                      , _level   :: !Trans.Level
                      }
           deriving Show
makeLenses ''Entry

baseTmap :: TypeMap
baseTmap = Map.fromList [(S.intern "int",    PT.INT)
                        ,(S.intern "string", PT.STRING)
                        ,(S.intern "nil",    PT.NIL)]

baseEmap :: EntryMap
baseEmap = Map.fromList [(S.intern "print",     FunEntry [PT.STRING] PT.UNIT   Trans.outerMost)
                        ,(S.intern "flush",     FunEntry []          PT.UNIT   Trans.outerMost)
                        ,(S.intern "getchar",   FunEntry []          PT.STRING Trans.outerMost)
                        ,(S.intern "ord",       FunEntry [PT.STRING] PT.INT    Trans.outerMost)
                        ,(S.intern "chr",       FunEntry [PT.INT]    PT.STRING Trans.outerMost)
                        ,(S.intern "size",      FunEntry [PT.STRING] PT.INT    Trans.outerMost)
                        ,(S.intern "not",       FunEntry [PT.INT]    PT.INT    Trans.outerMost)
                        ,(S.intern "exit",      FunEntry [PT.INT]    PT.UNIT   Trans.outerMost)
                        ,(S.intern "substring", FunEntry [PT.STRING, PT.INT, PT.INT] PT.STRING Trans.outerMost)
                        ,(S.intern "concat",    FunEntry [PT.STRING, PT.STRING]      PT.STRING Trans.outerMost)
                        ]
