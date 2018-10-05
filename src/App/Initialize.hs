module App.Initialize where

import App.Environment
import Frame.CurrentMachine

genEnv = Env <$> genRegisters
