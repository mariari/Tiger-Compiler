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
  , initAllocRegs
  , string
  , sayName
  , regmap
  ) where

import Prelude hiding (exp)
import Control.Lens
import Control.Monad(foldM)
import System.IO.Unsafe(unsafePerformIO)
import Data.Symbol(Symbol)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as M

import qualified Semantic.Temp       as T
import qualified Frame.Interface     as I
import qualified IR.Tree             as Tree
import qualified Generation.Assembly as A
import Frame.X86Typ
import App.Environment

regmap :: (HasRegs s Registers, MonadReader s m) => m (M.HashMap T.Temp String)
regmap = do
  regs <- view regs <$> ask
  return $ M.fromList [ (regs^.fp,  "fp")
                      , (regs^.rax, "rax")
                      , (regs^.rcx, "rcx")
                      , (regs^.rbx, "rbx")
                      , (regs^.rdx, "rdx")
                      , (regs^.rsi, "rsi")
                      , (regs^.rdi, "rdi")
                      , (regs^.sp,  "sp")
                      , (regs^.r8,  "r8")
                      , (regs^.r9,  "r9")
                      , (regs^.r10, "r10")
                      , (regs^.r11, "r11")
                      , (regs^.r12, "r12")
                      , (regs^.r13, "r13")
                      , (regs^.r14, "r14")
                      , (regs^.r15, "r15")
                      ]
-- these are the registers which can be used for allocation
registers :: (MonadReader s m, HasRegs s Registers) => m [T.Temp]
registers = do
  env <- ask
  return $ fmap (\x -> view (regs . x) env) [rax, rcx, rdx, rbx, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15]

initAllocRegs :: (MonadReader s m, HasRegs s Registers) => m (M.HashMap T.Temp T.Temp)
initAllocRegs = M.fromList . fmap (\x -> (x,x)) <$> registers

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

string :: T.Label -> String -> String
string lab str = show lab <> ": db " <> show str

sayName :: (HasRegs s Registers, MonadReader s m) => T.Temp -> m String
sayName temp = do
  regMap <- regmap
  case M.lookup temp regMap of
    Just reg -> return reg
    Nothing  -> return (show temp)

-- fix up later
procEntryExit1 _ body = body

-- A very bare bones version of procEntryexit2
procEntryExit2 f body = do
  env        <- ask
  caleesaved <- callerSaved
  return (body <> [A.Oper {A.assem = "", A.dsts = [], A.srcs = env^.regs.rv : caleesaved, A.jump = Just []}])
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
