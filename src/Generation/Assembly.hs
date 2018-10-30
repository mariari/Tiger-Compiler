{-# LANGUAGE NamedFieldPuns #-}

module Generation.Assembly where

import Semantic.Temp
import Data.Symbol(unintern)
import Data.Sequence (fromList, Seq(..), singleton, index)
import Data.Char(ord)
import Control.DeepSeq(force)

data Instr =
    Oper { assem :: String
         , dsts  :: [Temp]
         , srcs  :: [Temp]
         , jump  :: Maybe [Label]
         }
  | Label { assem :: String
          , lab   :: Label
          }
  | Move { assem :: String
         , dst   :: Temp
         , src   :: Temp
         }

-- force is to stop space leaks with the index function!
format :: (Temp -> String) -> Instr -> String
format sayTemp = \case
  Oper  {assem, dsts, srcs, jump = Nothing} -> force $ speak assem (fromList dsts) (fromList srcs) Empty
  Oper  {assem, dsts, srcs, jump = Just x}  -> force $ speak assem (fromList dsts) (fromList srcs) (fromList x)
  Move  {assem, dst, src}                   -> force $ speak assem (singleton dst) (singleton src) Empty
  Label {assem}                             -> assem
  where
    speak assem dsts srcs jump = f assem
      where f ('`' : 's' : i : rest) = sayTemp (indexOrd srcs i) <> f rest
            f ('`' : 'd' : i : rest) = sayTemp (indexOrd dsts i) <> f rest
            f ('`' : 'j' : i : rest) = show    (indexOrd jump i) <> f rest
            f ('`' : _ : rest)       = error "bad assembly format"
            f (c : rest)             = c : f rest
            f []                     = []
            indexOrd seq i = (index seq (ord i - ord '0'))
