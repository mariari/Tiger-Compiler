{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Frame.X86
  ( Access(..)
  , I.name
  , I.newFrame
  , I.formals
  , I.allocLocal
  , Frame
  , FrameI(..)
  , Registers
  , Regs
  , fp
  , rv
  , genRegisters
  , exp
  , wordSize
  , externalCall
  , procEntryExit1
  ) where

import Prelude hiding (exp)
import Control.Lens
import Control.Monad(foldM)
import System.IO.Unsafe(unsafePerformIO)
import Data.Symbol(Symbol)

import qualified Semantic.Temp   as T
import qualified Frame.Interface as I
import qualified IR.Tree         as Tree

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
  { _fp :: T.Temp
  , _rv :: T.Temp
  } deriving Show
makeLenses ''Registers

genRegisters :: IO Registers
genRegisters = Reg <$> genFp <*> T.newTemp

genFp :: IO T.Temp
genFp = T.newTemp

wordSize :: Int
wordSize = 4

allocFormal (allocd, xs) False = (\x -> (allocd, InReg x : xs)) <$> T.newTemp
allocFormal (allocd, xs) True  = return (succAlloc, InFrame (succAlloc * wordSize + wordSize) : xs)
  where
    succAlloc = allocd + 1

exp :: Access -> Tree.Exp -> Tree.Exp
exp (InFrame offset) t = Tree.Mem (Tree.Binop t Tree.Plus (Tree.Const offset))
exp (InReg register) _ = Tree.Temp register

externalCall :: Symbol -> [Tree.Exp] -> Tree.Exp
externalCall name args = Tree.Call (Tree.Name (T.nameLabel name)) args

-- fix up later
procEntryExit1 _ body = body

instance I.FrameFn Frame where
  name = name
  newFrame label bs = do
    (formalsAlloc, formals) <- foldM allocFormal (0,[]) bs
    return (Frame { formals       = formals
                  , _formalsAlloc = formalsAlloc
                  , _localsAlloc  = 0
                  , name          = label
                  })

instance I.FrameInter Frame Access where
  formals = formals
  allocLocal f False = (\x -> (f, InReg x)) <$> T.newTemp
  allocLocal f True  = return (f', InFrame (f'^.localsAlloc * wordSize))
    where
      f' = over localsAlloc succ f

type FrameI = I.FrameInter Frame Access
