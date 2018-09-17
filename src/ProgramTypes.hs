module ProgramTypes where

import           Data.Symbol
import qualified Data.Map.Strict as Map -- we use Map as we need ordering on the symbols

type Unique = Int

data Type = INT
          | STRING
          | RECORD [(Symbol, Type)] !Unique
          | ARRAY Type !Unique
          | NIL
          | UNIT
          | SELF !Symbol !Unique       -- my personal hack for self referential types
          | NAME !Symbol !(Maybe Type) -- was a ty option ref in the book
          deriving (Show,Eq)


type SymMap a = Map.Map Symbol a

data Test = Test Int B

type B = Test
