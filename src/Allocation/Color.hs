module Allocation.Color where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Graph
import           Control.Monad.Reader
import           Data.List
import           Data.Maybe(catMaybes)
import Debug.Trace

import Liveness.Live
import Semantic.Temp
import Frame.CurrentMachine
import App.Environment

type Allocation = M.Map Temp Register

color :: (MonadReader s m, HasRegs s Registers)
      => IGraph -> Allocation -> (Node -> Int) -> m (Allocation, [Temp])
color (IGraph graph tempToNode _) initAlloc spillCost = do
  regs <- registers
  let k      = length regs
      regSet = S.fromList regs
      simplify :: Gr a b -> (Gr a b, [Node])
      simplify g = (delNodes underN g, underN) -- [Node] is the workList
        where underN = fmap fst
                     . reverse
                     . sortOn snd
                     . filter ((< k) . snd)
                     . fmap (\x -> (x, deg g x))
                     $ nodes g
      minSpill :: Graph gr => gr a b -> (gr a b, Node) -- minimum works on first
      minSpill g = reOrg g . minimum . fmap (\x -> (spillCost x, x)) $ nodes g
      loop g allocMap spills works
        | isEmpty g   = f (f (allocMap, []) works) spills          -- the graph is empty
        | nodes == [] = loop g'' allocMap (spilled : spills) works -- we can't simplify the graph
        | otherwise   = loop g'  allocMap spills (nodes <> works)  -- we can simplify the graph
        where
          (g', nodes)    = simplify g
          (g'', spilled) = minSpill g' -- for the g == g' case
          f              = foldl' (\ (a,s) n -> selectRegister graph regSet n a s)
      (allocs, spills) = (loop graph M.empty [] [])
  return (allocMapToTemp graph allocs, (isJust . lab graph <$> spills))

reOrg :: Graph gr => gr a1 b -> (a2, Node) -> (gr a1 b, Node)
reOrg g (_, n) = (delNode n g, n)

-- | allocMapTotemp turns a map of nodes as keys to a map of Temps as keys
allocMapToTemp :: (Ord k2, Graph gr) => gr k2 b -> M.Map Node a -> M.Map k2 a
allocMapToTemp graph = M.mapKeys (isJust . lab graph)

isJust (Just x) = x
isJust Nothing  = error "an allocated variable is not in the interference map!"

-- | Select register allocates a node to the allocMap or adds it to the spillCost
-- Note that it should never spill unless we send in a potential spill node!
selectRegister :: (Graph gr, Ord a1) =>
  gr a2 b -> S.Set a1 -> Node -> M.Map Node a1 -> [Node] -> (M.Map Node a1, [Node])
selectRegister graph regSet node allocMap spillXs =
  case fasterDifference regSet takenColors  of
    []    -> (allocMap, node : spillXs)
    (x:_) -> (M.insert node x allocMap, spillXs )
  where
    takenColors = catMaybes (fmap (allocMap M.!?) (neighbors graph node))

-- | this is a set difference that converts the output back into a list
fasterDifference :: Ord a => S.Set a -> [a] -> [a]
fasterDifference set = S.toAscList . (set S.\\) . S.fromList
