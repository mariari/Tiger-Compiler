module ProgramTypes where

import           Data.Symbol
import           Data.IORef.Show
import qualified Data.Map.Strict as Map -- we use Map as we need ordering on the symbols

type Unique = Int

data Type = INT
          | STRING
          | RECORD [(Symbol, Type)] !Unique
          | ARRAY Type !Unique
          | NIL
          | UNIT
          | NAME !Symbol (IORef (Maybe Type)) -- no way to solve cyclic dependencies without a ref
          deriving (Show,Eq)


type SymMap a = Map.Map Symbol a
