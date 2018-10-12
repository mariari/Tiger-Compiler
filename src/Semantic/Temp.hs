module Semantic.Temp
  ( Temp
  , newTemp
  , Label
  , newLabel
  , nameLabel
  , fromLabel
  ) where

import Data.Symbol as S
import Data.Semigroup((<>))
import Data.Unique.Show

-- Temp Type ----------------------------------------------------------

newtype Temp = T Unique deriving (Eq,Ord)

newTemp = T <$> newUnique

instance Show Temp where
  show (T x) = "t" <> show (hashUnique x)

-- Label Type --------------------------------------------------------
newtype Label = L Symbol deriving Show

newLabel :: IO Label
newLabel = (\u -> L (S.intern $ "L" <> show (hashUnique u))) <$> newUnique

nameLabel :: Symbol -> Label
nameLabel = L

fromLabel :: Label -> Symbol
fromLabel (L s) = s
