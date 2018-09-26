module Data.IORef.Show (module Data.IORef) where

import Data.IORef

instance Show (IORef a) where
  show _ = "IOREF"
