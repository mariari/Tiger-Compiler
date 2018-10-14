module AbstractSyntax where

import Data.Symbol
import Data.IORef.Show

type Program = Exp

type Line = Int
type Column = Int

type Pos = (Line, Column)
type Escape = Bool

-- Bools are an escape parameter

data Exp = Var Var
         | Nil                                   {-# UNPACK #-} !Pos
         | IntLit    {-# UNPACK #-} !Int         {-# UNPACK #-} !Pos
         | StringLit {-# UNPACK #-} !String      {-# UNPACK #-} !Pos
         | Break                                 {-# UNPACK #-} !Pos
         | Sequence [Exp]                        {-# UNPACK #-} !Pos
         | Negation Exp                          {-# UNPACK #-} !Pos
         | Funcall !Symbol [Exp]                 {-# UNPACK #-} !Pos
         | Infix' Exp Op Exp                     {-# UNPACK #-} !Pos
         | ArrCreate !Symbol Exp Exp             {-# UNPACK #-} !Pos
         | RecCreate !Symbol [Field]             {-# UNPACK #-} !Pos
         | Assign Var Exp                        {-# UNPACK #-} !Pos
         | IfThenElse Exp Exp Exp                {-# UNPACK #-} !Pos
         | IfThen     Exp Exp                    {-# UNPACK #-} !Pos
         | While Exp Exp                         {-# UNPACK #-} !Pos
         | For Symbol (IORef Escape) Exp Exp Exp {-# UNPACK #-} !Pos
         | Let [Dec] [Exp]                       {-# UNPACK #-} !Pos
         deriving Show

data Field = Field { fieldTyp :: !Symbol
                   , expr     :: Exp
                   , pos      :: {-# UNPACK #-} !Pos
                   } deriving Show

data FieldDec = FieldDec !Symbol
                         !(IORef Escape)
                         !Symbol
                         {-# UNPACK #-} !Pos
              deriving Show

data Ty = NameTy   !Symbol {-# UNPACK #-} !Pos
        | ArrayTy  !Symbol {-# UNPACK #-} !Pos
        | RecordTy [FieldDec]
        deriving Show

-- non-left recursive
data Var = SimpleVar !Symbol     {-# UNPACK #-} !Pos
         | FieldVar  Var !Symbol {-# UNPACK #-} !Pos
         | Subscript Var Exp     {-# UNPACK #-} !Pos
         deriving Show


data Dec = FunDec !Symbol [FieldDec]     (Maybe Symbol) Exp {-# UNPACK #-} !Pos
         | VarDec !Symbol (IORef Escape) (Maybe Symbol) Exp {-# UNPACK #-} !Pos
         | TypeDec !Symbol Ty                               {-# UNPACK #-} !Pos
         deriving Show

data Op = Plus | Minus | Times | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or deriving (Show,Eq)
