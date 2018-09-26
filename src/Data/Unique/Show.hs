module Data.Unique.Show ( module Data.Unique ) where

import Data.Monoid((<>))
import Data.Unique

instance Show Unique where
  show x = "Unique { hash: " <> show (hashUnique x) <> " }"
