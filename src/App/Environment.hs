{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module App.Environment where

import Control.Lens

import Frame.CurrentMachine

data Env = Env
  { envRegs :: Registers }

makeLensesWith camelCaseFields ''Env
