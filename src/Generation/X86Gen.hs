module Generation.X86Gen (codegen) where

import Control.Monad.State.Strict

import qualified Frame.X86 as F
import qualified IR.Tree as T
import Data.Symbol(unintern)

import Semantic.Temp
import Generation.Assembly(Instr(..))


-- note for X86 we can ignore the frame!
codegen frame stm = undefined

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
binopToInst T.Plus    = "ADD"
binopToInst T.Minus   = "SUB"
binopToInst T.Mul     = "IMUL" -- mult is singed
binopToInst T.Div     = "IDIV"
binopToInst T.And     = "AND"
binopToInst T.Or      = "OR"
binopToInst T.Xor     = "XOR"
binopToInst T.LShift  = "SHL" -- H for logical shifts
binopToInst T.RShift  = "SHR"
binopToInst T.ARShift = "SAR"  -- A is for arithmetic shift

showPlusOrMin :: (Num a, Ord a, Show a) => a -> String
showPlusOrMin x
  | x >= 0    = "+" <> show x
  | otherwise = show x


-- | emits an instruction onto the instructions list while munching the IR
emit :: MonadState [a] m => a -> m ()
emit = modify . (:)

-- | munchStm adds assembly instructions to the state as a side effect as it evaluates statements
munchStm :: MonadState [Instr] m => T.Stmt -> m ()
munchStm (T.Seq s1 s2)        = munchStm s1 *> munchStm s2
munchStm (T.Exp (T.Const {})) = return ()
munchStm (T.Exp e)            = munchExp e *> return ()
munchStm (T.Label label)      = emit $ Label {assem = show label <> ": ", lab = label}
munchStm (T.CJump relop e1 e2 t f) = do
  me1 <- munchExp e1
  me2 <- munchExp e2
  emit $ Oper { assem = "CMP `s0, `s1\n" <> relopToJmp relop <> " j0\n" <> "JMP `j1\n"
              , srcs  = [me1, me2]
              , dsts  = []
              , jump  = Just [t,f]
              }
munchStm (T.Jump (T.Name j) ls) =
  emit $ Oper {assem = "JMP `j0\n"
              , srcs = []
              , dsts = []
              , jump = Just [j]
              }
munchStm (T.Jump e ls) = do
  me <- munchExp e
  emit $ Oper { assem = "JMP `j0\n"
              , srcs  = [me]
              , dsts  = []
              , jump  = Just ls}
-- time for the big set of moves
munchStm (T.Move (T.Temp d) (T.Temp s)) =
  emit $ Move {assem = "MOV `d0, `s0", dst = d, src = s}
munchStm (T.Move (T.Temp d) x) =
  case x of
    T.Mem (T.Binop (T.Temp s)  T.Plus  (T.Const c)) -> handleTempConst d s c
    T.Mem (T.Binop (T.Const c) T.Plus  (T.Temp s))  -> handleTempConst d s c
    T.Mem (T.Binop (T.Temp s)  T.Minus (T.Const c)) -> handleTempConst d s (-1 * c)
    -- don't handle the minus with const first as it needs to be done explicitly
    T.Mem (T.Binop (T.Temp c)  T.Plus  (T.Temp s))  ->
      emit $ Oper { assem = "MOV `d0, [`s0+`s1]\n"
                  , srcs  = [s]
                  , dsts  = [d]
                  , jump  = Nothing
                  }
    T.Mem (T.Binop (T.Temp c) T.Minus (T.Temp s)) ->
      emit $ Oper { assem = "MOV `d0, [`s0-`s1]\n"
                  , srcs  = [s]
                  , dsts  = [d]
                  , jump  = Nothing
                  }
    e -> do
      me <- munchExp e
      emit $ Oper { assem = "MOV `d0, `s0\n"
                  , srcs  = [me]
                  , dsts  = [d]
                  , jump  = Nothing
                  }
munchStm (T.Move (T.Mem (T.Temp d)) e) = do
  me <- munchExp e
  emit $ Oper { assem = "MOV DWORD PTR [`d0], `s0\n"
              , srcs  = [me]
              , dsts  = [d]
              , jump  = Nothing
              }
munchStm (T.Move (T.Mem (T.Binop (T.Temp d0) T.Plus (T.Temp d1))) e) = do
  me <- munchExp e
  emit $ Oper { assem = "MOV DWORD PTR [`d0+`d1], `s0\n"
              , srcs  = [me]
              , dsts  = [d0,d1]
              , jump  = Nothing
              }
munchStm (T.Move (T.Mem (T.Binop (T.Temp d) T.Plus (T.Const c1))) (T.Const c2)) = handleMemTwoConst d c1 c2
munchStm (T.Move (T.Mem (T.Binop (T.Const c1) T.Plus (T.Temp d))) (T.Const c2)) = handleMemTwoConst d c1 c2
munchStm (T.Move (T.Mem (T.Binop (T.Temp d) T.Plus (T.Const c))) e)  = handleMemConst d e c
munchStm (T.Move (T.Mem (T.Binop (T.Const c) T.Plus (T.Temp d))) e)  = handleMemConst d e c
munchStm (T.Move (T.Mem (T.Binop (T.Const c) T.Minus (T.Temp d))) e) = handleMemConst d e c
munchStm (T.Move (T.Mem d) e) = do
  md <- munchExp d
  me <- munchExp e
  emit $ Oper { assem = "MOV [`d0], `s0\n"
              , srcs  = [me]
              , dsts  = [md]
              , jump  = Nothing
              }
munchStm (T.Move {}) = error " munchStm has received an improper Move Code"

handleTempConst :: MonadState [Instr] m => Temp -> Temp -> Int -> m ()
handleTempConst d s c =
  emit $ Oper { assem = "MOV `d0, [`s0" <> showPlusOrMin c <> "]\n"
              , srcs  = [s]
              , dsts  = [d]
              , jump  = Nothing
              }

handleMemConst :: MonadState [Instr] m => Temp -> T.Exp -> Int -> m ()
handleMemConst d e c = do
  me <- munchExp e
  emit $ Oper { assem = "MOV DWORD PTR [`d0" <> showPlusOrMin c <> "], `s0\n"
              , srcs  = [me]
              , dsts  = [d]
              , jump  = Nothing
              }

handleMemTwoConst :: MonadState [Instr] m => Temp -> Int -> Int -> m ()
handleMemTwoConst d c1 c2 = do
  emit $ Oper { assem = "MOV DWORD PTR [`d0" <> showPlusOrMin c1 <> "], " <> show c2 <> "\n"
              , srcs  = []
              , dsts  = [d]
              , jump  = Nothing
              }

-- | munchExp adds assembly instructions to the state as a side effect as it evaluates expressions
-- munchExp also returns the result in a temp
munchExp exp = undefined
