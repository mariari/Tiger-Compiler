module App.Initialize where

import App.Environment
import Frame.CurrentMachine

import Data.IORef

genEnv = Env <$> genRegisters
             <*> newIORef []
