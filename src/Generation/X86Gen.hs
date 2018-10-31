module Generation.X86Gen (codegen) where

import Control.Monad.State.Strict

import Control.Lens
import Data.Symbol(unintern)
import Control.Monad.Reader

import qualified IR.Tree as T
import Frame.X86 as F
import App.Environment
import Semantic.Temp
import Generation.Assembly(Instr(..))

type MonadGen m = ( MonadState [Instr] m
                  , MonadReader Env m
                  , MonadIO m
                  )

-- note for X86 we can ignore the frame!
codegen :: (MonadReader Env m, MonadIO m) => p -> T.Stmt -> m [Instr]
codegen frame stm = reverse <$> execStateT (munchStm stm) []

-- Helper functions------------------------------------------------------------------

-- should probably change the String to Text at some point!

relopToJmp :: T.Relop -> String
relopToJmp T.Eq  = "JE"
relopToJmp T.Ne  = "JNE"
relopToJmp T.Lt  = "JL"
relopToJmp T.Le  = "JLE"
relopToJmp T.Gt  = "JG"
relopToJmp T.Ge  = "JGE"
relopToJmp T.ULt = "JB"
relopToJmp T.ULe = "JBE"
relopToJmp T.UGt = "JA"
relopToJmp T.UGe = "JAE"

binopToInst :: T.BinOp -> String
binopToInst T.Plus    = "ADDQ"
binopToInst T.Minus   = "SUBQ"
binopToInst T.Mul     = "IMUL" -- mult is singed
binopToInst T.Div     = "IDIV"
binopToInst T.And     = "ANDQ"
binopToInst T.Or      = "ORQ"
binopToInst T.Xor     = "XORQ"
binopToInst T.LShift  = "SHLQ" -- H for logical shifts
binopToInst T.RShift  = "SHRQ"
binopToInst T.ARShift = "SARQ"  -- A is for arithmetic shift

showPlusOrMin :: (Num a, Ord a, Show a) => a -> String
showPlusOrMin x
  | x >= 0    = "+" <> show x
  | otherwise = show x

-- | emits an instruction onto the instructions list while munching the IR
emit :: MonadState [a] m => a -> m ()
emit = modify . (:)

-- | a default operator instruction to reduce repeat code!
defOp :: Instr
defOp = Oper { assem = "", srcs = [], dsts = [], jump = Nothing}

-- Munches a mem call, is used in both munchStm and munchExp
munchMem x r = case x of
  T.Mem (T.Binop (T.Temp s)  T.Plus  (T.Const c)) -> handleTempConst r s c
  T.Mem (T.Binop (T.Const c) T.Plus  (T.Temp s))  -> handleTempConst r s c
  T.Mem (T.Binop (T.Temp s)  T.Minus (T.Const c)) -> handleTempConst r s (-1 * c)
    -- don't handle the minus with const first as it needs to be done explicitly
  T.Mem (T.Binop (T.Temp c)  T.Plus  (T.Temp s))  ->
    emit $ defOp { assem = "MOVQ `d0, [`s0+`s1]\n"
                 , srcs  = [s]
                 , dsts  = [r]
                 }
  T.Mem (T.Binop (T.Temp c) T.Minus (T.Temp s)) ->
    emit $ defOp { assem = "MOVQ `d0, [`s0-`s1]\n"
                 , srcs  = [s]
                 , dsts  = [r]
                 }
  e -> do
    me <- munchExp e
    emit $ defOp { assem = "MOVQ `d0, `s0\n"
                 , srcs  = [me]
                 , dsts  = [r]
                 }

-- might have missed some cases with a generic d on the lefthand side of Move
-- | munchStm adds assembly instructions to the state as a side effect as it evaluates statements
munchStm :: (MonadGen m) => T.Stmt -> m ()
munchStm (T.Seq s1 s2)        = munchStm s1 *> munchStm s2
munchStm (T.Exp (T.Const {})) = return ()
munchStm (T.Exp e)            = munchExp e *> return ()
munchStm (T.Label label)      = emit $ Label {assem = show label <> ": ", lab = label}
munchStm (T.CJump relop e1 e2 t f) = do
  me1 <- munchExp e1
  me2 <- munchExp e2
  emit $ defOp { assem = "CMPQ `s0, `s1\n"
               , srcs  = [me1, me2]
               }
  emit $ defOp { assem = relopToJmp relop <> " j0\n" <> "JMP `j1\n"
               , jump  = Just [t,f]
               }
munchStm (T.Jump (T.Name j) ls) =
  emit $ defOp { assem = "JMP `j0\n"
               , jump = Just [j] }
munchStm (T.Jump e ls) = do
  me <- munchExp e
  emit $ defOp { assem = "JMP `j0\n"
              , srcs  = [me]
              , jump  = Just ls
              }
-- time for the big set of moves
munchStm (T.Move (T.Temp d) (T.Temp s)) = emit Move {assem = "MOVQ `d0, `s0", dst = d, src = s}
munchStm (T.Move (T.Temp d) x)          = munchMem x d
munchStm (T.Move (T.Mem (T.Temp d)) e) = do
  me <- munchExp e
  emit $ defOp { assem = "MOVQ QWORD PTR [`d0], `s0\n"
               , srcs  = [me]
               , dsts  = [d]
               }
munchStm (T.Move (T.Mem (T.Binop (T.Temp d0) T.Plus (T.Temp d1))) e) = do
  me <- munchExp e
  emit $ defOp { assem = "MOVQ QWORD PTR [`d0+`d1], `s0\n"
               , srcs  = [me]
               , dsts  = [d0,d1]
               }
munchStm (T.Move (T.Mem (T.Binop (T.Temp d) T.Plus (T.Const c1))) (T.Const c2)) = handleMemTwoConst d c1 c2
munchStm (T.Move (T.Mem (T.Binop (T.Const c1) T.Plus (T.Temp d))) (T.Const c2)) = handleMemTwoConst d c1 c2
munchStm (T.Move (T.Mem (T.Binop (T.Temp d) T.Plus (T.Const c))) e)  = handleMemConst d e c
munchStm (T.Move (T.Mem (T.Binop (T.Const c) T.Plus (T.Temp d))) e)  = handleMemConst d e c
munchStm (T.Move (T.Mem (T.Binop (T.Const c) T.Minus (T.Temp d))) e) = handleMemConst d e c
munchStm (T.Move (T.Mem d) e) = do
  md <- munchExp d
  me <- munchExp e
  emit $ defOp { assem = "MOVQ [`d0], `s0\n"
               , srcs  = [me]
               , dsts  = [md]
               }
munchStm (T.Move {}) = error " munchStm has received an improper Move Code"

-- a few helpers for munchStm----------------------------------------------------------------
handleTempConst :: MonadState [Instr] m => Temp -> Temp -> Int -> m ()
handleTempConst d s c =
  emit defOp { assem = "MOVQ `d0, [`s0" <> showPlusOrMin c <> "]\n"
             , srcs  = [s]
             , dsts  = [d]
             }

handleMemConst :: MonadGen m => Temp -> T.Exp -> Int -> m ()
handleMemConst d e c = do
  me <- munchExp e
  emit $ defOp { assem = "MOVQ QWORD PTR [`d0" <> showPlusOrMin c <> "], `s0\n"
               , srcs  = [me]
               , dsts  = [d]
               }

handleMemTwoConst :: MonadState [Instr] m => Temp -> Int -> Int -> m ()
handleMemTwoConst d c1 c2 = do
  emit $ defOp { assem = "MOVQ QWORD PTR [`d0" <> showPlusOrMin c1 <> "], " <> show c2 <> "\n"
               , dsts  = [d]
               }
-- Munch Exp--------------------------------------------------------------------------------

-- | munchExp adds assembly instructions to the state as a side effect as it evaluates expressions
-- munchExp also returns the result in a temp
munchExp :: MonadGen m => T.Exp -> m Temp
munchExp (T.ESeq s e) = munchStm s >> munchExp e
munchExp (T.Temp t)   = return t
munchExp x@(T.Mem {}) = result (munchMem x)
munchExp (T.Const c)  = result $ \r ->
  emit $ defOp { assem = "MOVQ `d0, " <> show c <> "\n"
               , dsts  = [r]
               }
munchExp (T.Name l) = result $ \r ->
  emit $ defOp { assem = "LEA `d0 [" <> show l <> "]\n"
               , dsts  = [r]
               , jump  = Just [l]
               }
munchExp (T.Neg (T.Const c)) = result $ \r -> do
  emit $ defOp { assem = "MOVQ `d0, " <> show c <> "\n"
               , dsts  = [r]
               }
  emit $ defOp { assem = "NEG `s0\n"
               , srcs  = [r]
               }
munchExp (T.Neg e) = result $ \r -> do
  me <- munchExp e
  emit $ Move { assem = "MOVQ `d0, `s0\n"
              , dst  = r
              , src  = me
              }
  emit $ defOp { assem = "NEG `s0\n"
               , srcs  = [r]
               }
munchExp (T.Binop e1 T.Div e2) = result $ \r -> do
  me1 <- munchExp e1
  env <- ask
  emit $ Move { assem = "MOVQ `d0, `s0\n"
              , dst   = env^.regs.rax
              , src   = me1
              }
  me2 <- munchExp e2
  emit $ defOp { assem = "IDIV `s0\n"
               , srcs = [me2]
               } -- removed [env^.regs.rax, env^.regs.rdx] as it isn't used here!
  emit $ Move { assem = "MOVQ `s0, `d0\n"
              , dst   = r
              , src   = env^.regs.rax
              }
munchExp (T.Binop e1 op e2) = result $ \r -> do
  me1 <- munchExp e1
  env <- ask
  emit $ Move { assem = "MOV `d0, `s0\n"
              , src   = me1
              , dst   = r
              }
  me2 <- munchExp e2
  emit defOp { assem = binopToInst op <> " `d0, `s0\n"
             , dsts  = [r]
             , srcs  = [me2]
             }
munchExp (T.Call (T.Name l) args) = result $ \r -> do
  env     <- ask
  mArgs   <- traverse munchExp args
  argRegs <- argumentRegs
  let argsOverFlow = length mArgs - length argRegs -- this will detect overflow in the #regs
  -- only happens when argsOverFlow > 0
  traverse (\a -> emit defOp {assem = "PUSHQ `s0\n", srcs = [a]})
           (take argsOverFlow mArgs)
  zipWithM (\a r -> emit  Move {assem = "MOVQ `s0, `d0", src = a, dst = r})
           (drop argsOverFlow mArgs)
           argRegs
  callerS <- callerSaved
  emit defOp { assem = "CALLQ " <> show l <> "\n" }
  when (argsOverFlow > 0)
       (emit defOp { assem = "ADDQ " <> showPlusOrMin (wordSize * argsOverFlow) <>  ", `d0\n"
                   , dsts  = [env^.regs.sp] })
  emit Move { assem = "MOVQ `s0 `d0\n"
            , src   = env^.regs.rv
            , dst   = r
            }
munchExp (T.Call exp _) = error ("munchExp has gotten a bad call with exp " <> show exp)
-- Munch Exp Helpers------------------------------------------------------------------------
result :: MonadIO m => (Temp -> m a) -> m Temp
result gen = do
  t <- liftIO newTemp
  gen t
  return t
