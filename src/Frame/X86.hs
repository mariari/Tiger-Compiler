{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Frame.X86
  ( Access(..)
  , I.name
  , I.newFrame
  , I.formals
  , I.allocLocal
  , Frame
  ) where

import           Control.Monad(foldM)
import           Control.Lens
import qualified Semantic.Temp   as T
import qualified Frame.Interface as I

data Access = InFrame Int
            | InReg   T.Temp
            deriving Show

data F = F { _formals      :: [Access]
           , _formalsAlloc :: Int
           , _localsAlloc  :: Int
           , _name         :: T.Label
           }
makeLenses ''F

wordSize :: Int
wordSize = 4

allocFormal (allocd, xs) False = (\x -> (allocd, InReg x : xs)) <$> T.newTemp
allocFormal (allocd, xs) True  = return (succAlloc, InFrame (succAlloc * wordSize + wordSize) : xs)
  where
    succAlloc = allocd + 1

instance I.FrameFn F where
  name = view name

  newFrame label bs = do
    (formalsAlloc, formals) <- foldM allocFormal (0,[]) bs
    return (F { _formals      = formals
              , _formalsAlloc = formalsAlloc
              , _localsAlloc  = 0
              , _name         = label
              })

instance I.Frame F Access where
  formals = view formals

  allocLocal f False = (\x -> (f ,InReg x)) <$> T.newTemp
  allocLocal f True  = return (f', InFrame (f'^.localsAlloc * wordSize))
    where
      f' = over localsAlloc succ f

type Frame = I.Frame F Access
