{-# LANGUAGE TemplateHaskell #-}

module Frame.X86Typ where

import Control.Lens

import qualified Semantic.Temp as T

data Access = InFrame Int
            | InReg   T.Temp
            deriving Show

data Frame = Frame { formals       :: [Access]
                   , _formalsAlloc :: Int
                   , _localsAlloc  :: Int
                   , name          :: T.Label
                   } deriving Show
makeLenses ''Frame

type Regs = Registers

data Registers = Reg
  { _fp  :: T.Temp -- frame pointer rbp
  , _rv  :: T.Temp
  , _rax :: T.Temp
  , _rcx :: T.Temp
  , _rdx :: T.Temp
  , _rbx :: T.Temp
  , _rsi :: T.Temp
  , _rdi :: T.Temp
  , _sp  :: T.Temp -- stack pointer rsp
  , _r8  :: T.Temp
  , _r9  :: T.Temp
  , _r10 :: T.Temp
  , _r11 :: T.Temp
  , _r12 :: T.Temp
  , _r13 :: T.Temp
  , _r14 :: T.Temp
  , _r15 :: T.Temp
  } deriving Show
makeLenses ''Registers


genRegisters :: IO Registers
genRegisters =
  (\x -> Reg x x) <$> T.newTemp <*> T.newTemp
                  <*> T.newTemp <*> T.newTemp
                  <*> T.newTemp <*> T.newTemp
                  <*> T.newTemp <*> T.newTemp
                  <*> T.newTemp <*> T.newTemp
                  <*> T.newTemp <*> T.newTemp
                  <*> T.newTemp <*> T.newTemp
                  <*> T.newTemp <*> T.newTemp
