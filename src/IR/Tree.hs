module IR.Tree where

import qualified Semantic.Temp as Temp

data Exp = Const Int
         | Neg Exp
         | Name Temp.Label
         | Temp Temp.Temp
         | Binop Exp BinOp Exp
         | Mem Exp -- augment with Size later
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
           | Xor
           | LShift
           | RShift
           | ARShift
           deriving (Eq,Show,Enum)

notRel :: Relop -> Relop
notRel Eq  = Ne
notRel Ne  = Eq
notRel Lt  = Ge
notRel Ge  = Lt
notRel Gt  = Le
notRel Le  = Gt
notRel ULt = UGe
notRel UGe = ULt
notRel UGt = ULe
notRel ULe = UGt
