module Semantic.Temp
  ( Temp
  , newTemp
  , Label
  , newLabel
  , nameLabel
  ) where

import Data.Symbol as S
import Data.Semigroup((<>))
import Data.Unique.Show

-- Temp Type ----------------------------------------------------------

newtype Temp = T Unique

newTemp = T <$> newUnique

instance Show Temp where
  show (T x) = "t" <> show (hashUnique x)

-- Label Type --------------------------------------------------------
newtype Label = L Symbol deriving Show

newLabel :: IO Label
newLabel = (\u -> L (S.intern $ "L" <> show (hashUnique u))) <$> newUnique

nameLabel :: Symbol -> Label
nameLabel = L
