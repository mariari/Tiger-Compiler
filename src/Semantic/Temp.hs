module Semantic.Temp
  ( Label(..)
  , Temp
  ) where


import Data.Symbol

type Label = Symbol

newtype Temp = Temp Int
