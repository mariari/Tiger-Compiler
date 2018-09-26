module Data.IORef.Show where

import Data.IORef

instance Show (IORef a) where
  show _ = "IOREF"
