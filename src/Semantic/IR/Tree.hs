module Semantic.IR.Tree where

import qualified Semantic.Temp as Temp

data Exp = Const Int
         | Name Temp.Label
         | Temp Temp.Temp
         | Binop Exp BinOp Exp
         | Mem Exp
         | Call Exp [Exp]
         | ESeq Stmt Exp
         deriving Show

data Stmt = Move Exp Exp
          | Exp Exp
          | Jump Exp [Temp.Label]
          | CJump Relop Exp Exp Temp.Label Temp.Label
          | Seq Stmt Stmt
          | Label Temp.Label
          deriving Show

-- U for unsigned
data Relop = Eq | Ne | Lt | Gt | Le | Ge | ULt | ULe | UGt | UGe deriving (Eq,Show,Enum)

data BinOp = Plus
           | Minus
           | Mul
           | Div
           | And
           | Or
           | LShift
           | RShift
           | ARShift
           | Xor
           deriving (Eq,Show,Enum)
