{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module App.Environment where

import Control.Lens
import Data.IORef.Show

import Frame.CurrentMachineTyp
import Semantic.Fragment

data Env = Env
  { envRegs :: Registers
  , envFrag :: IORef [Frag] } deriving Show

-- can be used to test Registers without mocking frag
data MinimalEnv = MinimalEnv
  { minimalEnvRegs :: Registers } deriving Show

makeLensesWith camelCaseFields ''Env
makeLensesWith camelCaseFields ''MinimalEnv
