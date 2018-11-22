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
  , fp
  , rv
  , sp
  , rax
  , Registers
  , Register
  , Regs
  , Reg
  , rdx
  , exp
  , wordSize
  , externalCall
  , procEntryExit1
  , procEntryExit2
  , argumentRegs
  , callerSaved
  , registers
  ) where

import Prelude hiding (exp)
import Control.Lens
import Control.Monad(foldM)
import System.IO.Unsafe(unsafePerformIO)
import Data.Symbol(Symbol)
import Control.Monad.Reader

import qualified Semantic.Temp       as T
import qualified Frame.Interface     as I
import qualified IR.Tree             as Tree
import qualified Generation.Assembly as A
import Frame.X86Typ
import App.Environment

-- these are the registers which can be used for allocation
registers :: (MonadReader s m, HasRegs s Registers) => m [T.Temp]
registers = do
  env <- ask
  return $ fmap (\x -> view (regs . x) env) [rax, rcx, rdx, rbx, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15]

callerSaved :: (MonadReader s m, HasRegs s Registers) => m [T.Temp]
callerSaved = do
  env <- ask
  return $ fmap (\x -> view (regs . x) env) [rax, rcx, rdx, r8, r9, r10, r11]

calleeSaved :: (MonadReader s m, HasRegs s Registers) => m [T.Temp]
calleeSaved = do
  env <- ask
  return $ fmap (\x -> view (regs . x) env) [sp, fp, rbx, rdx, rdi, rsi, r12, r13, r14, r15]

argumentRegs :: (MonadReader s m, HasRegs s Registers) => m [T.Temp]
argumentRegs = do
  env <- ask
  return $ fmap (\x -> view (regs . x) env) [rdi, rsi, rdx, rcx, r8, r9]

wordSize :: Int
wordSize = 8

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

-- A very bare bones version of procEntryexit2
procEntryExit2 f body = do
  env        <- ask
  caleesaved <- callerSaved
  return A.Oper {A.assem = "", A.dsts = [], A.srcs = env^.regs.rv : caleesaved, A.jump = Just []}
--  T.new

instance I.FrameInter Frame Access where
  name = name
  newFrame label bs = do
    (formalsAlloc, formals) <- foldM allocFormal (0,[]) bs
    return Frame { formals       = formals
                 , _formalsAlloc = formalsAlloc
                 , _localsAlloc  = 0
                 , name          = label
                 }
  formals = formals
  allocLocal f False = (\x -> (f, InReg x)) <$> T.newTemp
  allocLocal f True  = return (f', InFrame (f'^.localsAlloc * wordSize))
    where
      f' = over localsAlloc succ f

type FrameI = I.FrameInter Frame Access
