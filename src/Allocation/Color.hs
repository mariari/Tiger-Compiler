module Allocation.Color(color) where

import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Graph
import           Control.Monad.Reader
import           Data.List(sortOn, foldl')
import           Data.Maybe(catMaybes, isJust)
import           Control.Arrow((&&&), (***), second)

import Liveness.Live
import Semantic.Temp
import Frame.CurrentMachine
import App.Environment

type Allocation = M.HashMap Temp Register

-- Note color does not do coalescing, so optimizations can be had here
color :: (MonadReader s m, HasRegs s Registers)
      => IGraph -> Allocation -> (Node -> Double) -> m (Allocation, [Temp])
color (IGraph graph tempToNode _) initAlloc spillCost = do
  regs <- registers
  let k      = length regs
      regSet = S.fromList regs
      initMap = M.fromList
              . fmap (second fromJust)
              . filter (isJust . snd)
              . map (id &&& (`M.lookup` initAlloc) . fromJust . lab graph)
              $ nodes graph
      simplify :: Gr a b -> (Gr a b, [Node])
      simplify g = ((`delNodes` g) &&& id)
                 . fmap fst
                 . reverse
                 . sortOn snd
                 . filter ((< k) . snd)
                 . fmap (id &&& deg g)
                 $ nodes g
      minSpill :: Graph gr => gr a b -> (gr a b, Node) -- minimum works on first
      minSpill g = ((`delNode` g) &&& id) . snd . minimum . fmap (spillCost &&& id) $ nodes g
      loop g allocHashMap spills works
        | isEmpty g   = foldl' f (allocHashMap, []) [works, spills]    -- the graph is empty
        | nodes == [] = loop g'' allocHashMap (spilled : spills) works -- we can't simplify the graph
        | otherwise   = loop g'  allocHashMap spills (nodes <> works)  -- we can simplify the graph
        where
          (g', nodes)    = simplify g
          (g'', spilled) = minSpill g' -- for the g == g' case
          f              = foldl' (\ (a,s) n -> selectRegister graph regSet n a s)
      (allocs, spills) = (loop graph initMap [] [])
  return (allocHashMapToTemp graph allocs, (fromJust . lab graph <$> spills))

-- | allocHashMapTotemp turns a map of nodes as keys to a map of Temps as keys
allocHashMapToTemp :: (Eq k, Hashable k, Graph gr) => gr k b -> M.HashMap Node v -> M.HashMap k v
allocHashMapToTemp graph = mapKeys (fromJust . lab graph)

fromJust (Just x) = x
fromJust Nothing  = error "an allocated variable is not in the interference map!"

-- | Select register allocates a node to the allocHashMap or adds it to the spillCost
-- Note that it should never spill unless we send in a potential spill node!
selectRegister :: Ord a => Gr c b -> S.Set a -> Node -> M.HashMap Node a -> [Node] -> (M.HashMap Node a, [Node])
selectRegister graph regSet node allocHashMap spillXs
  | M.member node allocHashMap = (allocHashMap, spillXs)
  | otherwise =
    let takenColors = catMaybes ((`M.lookup` allocHashMap) <$> neighbors graph node) in
    case fasterDifference regSet takenColors of
      []    -> (allocHashMap, node : spillXs)
      (x:_) -> (M.insert node x allocHashMap, spillXs)

-- | this is a set difference that converts the output back into a list
fasterDifference :: Ord a => S.Set a -> [a] -> [a]
fasterDifference set = S.toAscList . (set S.\\) . S.fromList

-- taken from Data.Map.Strict Source
mapKeys :: (Eq k, Hashable k) => (t -> k) -> M.HashMap t v -> M.HashMap k v
mapKeys f = M.fromList . M.foldrWithKey (\k x xs -> (f k, x) : xs) []
{-# INLINABLE mapKeys #-}
