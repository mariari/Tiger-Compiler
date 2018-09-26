{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Frame.X86 where

import           Control.Lens
import qualified Semantic.Temp   as T
import qualified Frame.Interface as I

data Access = InFrame Int
            | InReg   T.Temp

data F = F { _formals      :: [Access]
           , _formalsAlloc :: Int
           , _localsAlloc  :: Int
           , _name         :: T.Label
           }

makeLenses ''F

wordSize = 4

instance I.Frame F Access where
  name    = view name
  formals = view formals

  allocLocal f False = ((,) f . InReg) <$> T.newTemp
  allocLocal f True  = return (f', InFrame (f'^.localsAlloc * wordSize))
    where
      f' = over localsAlloc succ f

  newFrame label bs = undefined
