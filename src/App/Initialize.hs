module App.Initialize where

import App.Environment
import Frame.CurrentMachineTyp

import Data.IORef

genEnv = Env <$> genRegisters
             <*> newIORef []
