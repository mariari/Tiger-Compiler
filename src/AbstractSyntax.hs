module AbstractSyntax where

import Data.Symbol
import Data.IORef.Show
import Control.Monad.Identity

type Program = Exp

type Line = Int
type Column = Int

type Pos = (Line, Column)
type Escape = Bool

type ExpI = Exp Identity

data Exp esc where
  Var        :: Var esc                                                      -> Exp esc
  Nil        ::                                          {-# UNPACK #-} !Pos -> Exp esc
  Break      ::                                          {-# UNPACK #-} !Pos -> Exp esc
  IntLit     :: {-# UNPACK #-} !Int                   -> {-# UNPACK #-} !Pos -> Exp esc
  StringLit  :: !String                               -> {-# UNPACK #-} !Pos -> Exp esc
  Sequence   :: [Exp esc]                             -> {-# UNPACK #-} !Pos -> Exp esc
  Negation   :: Exp esc                               -> {-# UNPACK #-} !Pos -> Exp esc
  Infix'     :: Exp esc -> !Op -> Exp esc             -> {-# UNPACK #-} !Pos -> Exp esc
  ArrCreate  :: !Symbol -> Exp esc -> Exp esc         -> {-# UNPACK #-} !Pos -> Exp esc
  RecCreate  :: !Symbol -> [Field esc]                -> {-# UNPACK #-} !Pos -> Exp esc
  Funcall    :: !Symbol -> [Exp esc]                  -> {-# UNPACK #-} !Pos -> Exp esc
  Assign     :: Var esc -> Exp esc                    -> {-# UNPACK #-} !Pos -> Exp esc
  IfThenElse :: Exp esc -> Exp esc -> Exp esc         -> {-# UNPACK #-} !Pos -> Exp esc
  IfThen     :: Exp esc -> Exp esc                    -> {-# UNPACK #-} !Pos -> Exp esc
  While      :: Exp esc -> Exp esc                    -> {-# UNPACK #-} !Pos -> Exp esc
  Let        :: [Dec esc] -> [Exp esc]                -> {-# UNPACK #-} !Pos -> Exp esc
  For        :: Show (esc Escape)
             => Symbol -> esc Escape
             -> Exp esc -> Exp esc -> Exp esc        -> {-# UNPACK #-} !Pos -> Exp esc

deriving instance Show (Exp esc)

data Field esc = Field { fieldTyp :: !Symbol
                       , expr     :: Exp esc
                       , pos      :: {-# UNPACK #-} !Pos
                       } deriving Show

data FieldDec esc where
  FieldDec :: Show (esc Escape)
           => !Symbol
           -> !(esc Escape)
           -> !Symbol
           -> {-# UNPACK #-} !Pos
           -> FieldDec esc
deriving instance Show (FieldDec esc)


data Ty esc = NameTy   !Symbol {-# UNPACK #-} !Pos
            | ArrayTy  !Symbol {-# UNPACK #-} !Pos
            | RecordTy [FieldDec esc]
            deriving Show

-- non-left recursive
data Var esc = SimpleVar !Symbol             {-# UNPACK #-} !Pos
             | FieldVar  (Var esc) !Symbol   {-# UNPACK #-} !Pos
             | Subscript (Var esc) (Exp esc) {-# UNPACK #-} !Pos
             deriving Show

data Dec esc where
  FunDec :: !Symbol
         -> [FieldDec esc]
         -> Maybe Symbol
         -> Exp esc
         -> {-# UNPACK #-} !Pos
         -> Dec esc
  VarDec :: Show (esc Escape)
         => !Symbol
         -> esc Escape
         -> Maybe Symbol
         -> Exp esc
         -> {-# UNPACK #-} !Pos
         -> Dec esc
  TypeDec :: !Symbol -> Ty esc -> {-# UNPACK #-} !Pos -> Dec esc

deriving instance Show (Dec esc)

data Op = Plus | Minus | Times | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or deriving (Show,Eq)
