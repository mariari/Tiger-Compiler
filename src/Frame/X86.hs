module Frame.X86 where

import qualified Semantic.Temp   as T
import qualified Frame.Interface as I

data Access = InFrame Int
            | InReg   T.Temp

data F = F { formals      :: [Access]
           , formalsAlloc :: Int
           , localsAlloc  :: Int
           , name         :: T.Label
           }


instance I.Frame F Access where
  newFrame label bs = undefined
