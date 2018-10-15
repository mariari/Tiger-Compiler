{-# LANGUAGE TemplateHaskell #-}

module Semantic.Environment where

import           Frame.X86
import qualified Semantic.Translate as Trans
import qualified Semantic.Temp      as Temp
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
                      , _label   :: !Temp.Label
                      }
           deriving Show
makeLenses ''Entry

baseTmap :: TypeMap
baseTmap = Map.fromList [(S.intern "int",    PT.INT)
                        ,(S.intern "string", PT.STRING)
                        ,(S.intern "nil",    PT.NIL)]

baseEmap ::  IO EntryMap
baseEmap = do
  l1  <- Temp.newLabel
  l2  <- Temp.newLabel
  l3  <- Temp.newLabel
  l4  <- Temp.newLabel
  l4  <- Temp.newLabel
  l5  <- Temp.newLabel
  l6  <- Temp.newLabel
  l7  <- Temp.newLabel
  l8  <- Temp.newLabel
  l9  <- Temp.newLabel
  l10 <- Temp.newLabel
  l11 <- Temp.newLabel
  return $
    Map.fromList [(S.intern "print",     FunEntry [PT.STRING] PT.UNIT   Trans.outerMost l1)
                 ,(S.intern "print_int", FunEntry [PT.INT]    PT.UNIT   Trans.outerMost l2)
                 ,(S.intern "flush",     FunEntry []          PT.UNIT   Trans.outerMost l3)
                 ,(S.intern "getchar",   FunEntry []          PT.STRING Trans.outerMost l4)
                 ,(S.intern "ord",       FunEntry [PT.STRING] PT.INT    Trans.outerMost l5)
                 ,(S.intern "chr",       FunEntry [PT.INT]    PT.STRING Trans.outerMost l6)
                 ,(S.intern "size",      FunEntry [PT.STRING] PT.INT    Trans.outerMost l7)
                 ,(S.intern "not",       FunEntry [PT.INT]    PT.INT    Trans.outerMost l8)
                 ,(S.intern "exit",      FunEntry [PT.INT]    PT.UNIT   Trans.outerMost l9)
                 ,(S.intern "substring", FunEntry [PT.STRING, PT.INT, PT.INT] PT.STRING Trans.outerMost l10)
                 ,(S.intern "concat",    FunEntry [PT.STRING, PT.STRING]      PT.STRING Trans.outerMost l11)
                 ]
