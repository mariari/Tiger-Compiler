module Liveness.Live --(interferenceGraph) where
  where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe
import           Data.Foldable(foldl')
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree

import Generation.Assembly hiding(lab)
import Semantic.Temp
import Liveness.Flow

type LiveMap = M.Map Node (S.Set Temp)

data IGraph = IGraph
  { graph      :: Gr Temp ()
  , tempToNode :: M.Map Temp Node
  , moves      :: [(Node, Node)]
  } deriving Show

interferenceGraph :: FlowGraph -> (IGraph, LiveMap)
interferenceGraph fgraph = (IGraph iGraph tempToNode moves, liveOutMap)
  where
    (_, liveOutMap)      = buildLiveMaps fgraph
    (iGraph, tempToNode) = ufold addEdges (empty, mempty) fgraph
    moves                = buildMoves fgraph tempToNode
    addEdges :: Context NodeInfo () -> (Gr Temp (), M.Map Temp Node) -> (Gr Temp (), M.Map Temp Node)
    addEdges (_, node, Node _ defs uses _, _) (igraph, tempNodeMap) =
      foldl' (\(iGr,tMap) def ->
                foldl' (\grMap lOut ->
                          let (iGr', tMap', from) = genOrGetNode grMap def
                              (iGr, tMap, to)     = genOrGetNode (iGr', tMap') lOut
                          in (if hasEdge iGr (from,to) then iGr else insEdge (from, to, ()) iGr, tMap))
                       (iGr, tMap)
                       (liveOutMap M.! node))
             (igraph, tempNodeMap)
             defs

-- Helper functions----------------------------------------------------------------

genOrGetNode :: (Ord k, DynGraph gr) => (gr k b, M.Map k Node) -> k -> (gr k b, M.Map k Node, Node)
genOrGetNode (iGraph, tempNodeMap) temp =
  case M.lookup temp tempNodeMap of
    Just n  -> (iGraph, tempNodeMap, n)
    Nothing ->
      let n = noNodes iGraph
      in (insNode (n, temp) iGraph, M.insert temp n tempNodeMap , n)

-- v defs and uses should only have 1 element in a move!
buildMoves :: FlowGraph -> M.Map Temp Node -> [(Node,Node)]
buildMoves fgraph tempToNode =
  catMaybes $
    fmap (\(_,Node _ [def] [use] _) -> (,) <$> tempToNode M.!? def <*> tempToNode M.!? use)
         (filter (isMove . snd) (labNodes fgraph))

-- | takes a flow graph and produces the tuple (LiveInMap, LiveOutMap)
buildLiveMaps :: FlowGraph -> (LiveMap, LiveMap)
buildLiveMaps g = recursive mempty mempty
  where
    nodes' = nodes g
    recursive inMap outMap
      | inMap == inMap' && outMap == outMap' = (inMap,outMap)
      | otherwise                           = recursive inMap' outMap'
      where
        (inMap', outMap') = foldr (f g) (inMap, outMap) nodes'
        f g n (i, o)      = (stepInMap n g i o, stepOutMap n g i o)

-- helper functions for buildLiveMaps-------------------------------------------------------

-- | does the inMap calculation once
stepInMap :: Node -> FlowGraph -> LiveMap -> LiveMap -> LiveMap
stepInMap node g inMap outMap = case match node g of
  (Nothing, g) -> inMap
  (Just (_, _, (Node _ def use _), _), g) ->
    let out = M.findWithDefault mempty node outMap
    in M.insert node (S.fromList use `S.union` (out S.\\ S.fromList def)) inMap

-- | does the outMap calculation once
stepOutMap :: Node -> FlowGraph -> LiveMap -> LiveMap -> LiveMap
stepOutMap node g inMap outMap = case match node g of
  (Nothing, g) -> inMap
  (Just (_, _, _, succ), g) ->
    M.insert node
             (S.unions (fmap (\((),n) -> M.findWithDefault mempty n inMap) succ))
             outMap
