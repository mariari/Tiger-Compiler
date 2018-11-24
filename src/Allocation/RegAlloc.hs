{-# LANGUAGE NamedFieldPuns #-}
module Allocation.RegAlloc where

import qualified Data.HashMap.Strict as M
import           Data.Graph.Inductive.Graph
import           Control.Monad.Reader
import           Data.Maybe(fromJust)

import Liveness.Flow
import Liveness.Live
import Semantic.Temp
import Frame.CurrentMachine
import App.Environment
import Allocation.Color
import Generation.Assembly hiding (lab)

type Allocation = M.HashMap Temp Register

-- Note alloc does **not** do spilling, and will resort in an error
alloc :: (MonadReader s m, HasRegs s Registers) => [Instr] -> Frame -> m ([Instr], Allocation)
alloc instrs frame = do
  let flowGraph     = instsToGrph instrs
      (igraph,live) = interferenceGraph flowGraph
      spillCost n   = fromInteger (ufold (f tempEle) 0 flowGraph) / fromIntegral (deg (graph igraph) n)
        where
          tempEle = fromJust (lab (graph igraph) n)
          f n (_, _, Node {def, use}, _) = (inUseSum n use + inUseSum n def +)
          inUseSum x xs | x `elem` xs = 1
                        | otherwise   = 0

  initRegMap       <- initAllocRegs
  (allocs, spills) <- color igraph initRegMap spillCost

  let isRedundant (Move {dst, src}) = allocs M.! dst == allocs M.! src
      isRedundant _                 = False
  if null spills
    then return (filter (not . isRedundant) instrs, allocs)
    else error "error in alloc, spilling isn't implemented yet!"
