module Semantic.Fragment where

import Semantic.Temp
import IR.Tree
import Frame.CurrentMachine

data Frag = Proc { body :: Stmt, f :: Frame }
          | Str Label String
          deriving Show
