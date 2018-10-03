{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Frame.X86
  ( Access(..)
  , I.name
  , I.newFrame
  , I.formals
  , I.allocLocal
  , Frame
  , FrameI(..)
  ) where

import           Control.Monad(foldM)
import           Control.Lens
import qualified Semantic.Temp   as T
import qualified Frame.Interface as I

data Access = InFrame Int
            | InReg   T.Temp
            deriving Show

data Frame = Frame { formals       :: [Access]
                   , _formalsAlloc :: Int
                   , _localsAlloc  :: Int
                   , name          :: T.Label
                   } deriving Show
makeLenses ''Frame

wordSize :: Int
wordSize = 4

allocFormal (allocd, xs) False = (\x -> (allocd, InReg x : xs)) <$> T.newTemp
allocFormal (allocd, xs) True  = return (succAlloc, InFrame (succAlloc * wordSize + wordSize) : xs)
  where
    succAlloc = allocd + 1

instance I.FrameFn Frame where
  name     = name
  newFrame label bs = do
    (formalsAlloc, formals) <- foldM allocFormal (0,[]) bs
    return (Frame { formals       = formals
                  , _formalsAlloc = formalsAlloc
                  , _localsAlloc  = 0
                  , name          = label
                  })

instance I.FrameInter Frame Access where
  formals    = formals
  allocLocal f False = (\x -> (f, InReg x)) <$> T.newTemp
  allocLocal f True  = return (f', InFrame (f'^.localsAlloc * wordSize))
    where
      f' = over localsAlloc succ f

type FrameI = I.FrameInter Frame Access
