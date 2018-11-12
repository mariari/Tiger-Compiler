module Liveness.Flow (instsToGrph,FlowGraph, NodeInfo(..)) where

import qualified Data.Map.Strict as M
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.Foldable(foldl')

import Generation.Assembly
import Semantic.Temp

-- this type is a big departure from the book as it uses many maps instead and claims to be more modular
-- though this technique should work just fine
data NodeInfo = Node
  { inst   :: Instr -- unsure if this is needed
  , def    :: [Temp]
  , use    :: [Temp]
  , isMove :: Bool
  } deriving Show

type FlowGraph = Gr NodeInfo ()

instsToGrph :: [Instr] -> FlowGraph
instsToGrph []     = empty
instsToGrph instrs = delEdge (num-1, num) g
  where
    (num,g,_) = foldl' f (0, empty, mempty) instrs
    -- the accumulator will be the node number, the flow graph,
    -- a map from labels to their nodes, note the label could possible not be in
    -- the list, as we can allocate this node before seeing the label!
    f :: (Int, FlowGraph, M.Map Label Node) -> Instr -> (Int, FlowGraph, M.Map Label Node)
    f (i, g, labMap) inst@(Oper asm dsts srcs Nothing) =
      let newNode = Node {inst = inst, def = dsts, use = srcs, isMove = False} in
      ( succ i
      , ([], i, newNode, [((), succ i)]) & g
      , labMap
      )
    f (i, g, labMap) inst@(Oper asm dsts srcs (Just xs)) =
      let newG = insNode (i, Node {inst = inst, def = dsts, use = srcs, isMove = False}) g
          fn x (newI, g, labMap) =
            case labMap M.!? x of
              Nothing   -> (succ newI, insEdge (i, succ newI, ()) g, labMap)
              Just node -> (newI     , insEdge (i, node     , ()) g, labMap)
      in
        foldr fn (i, newG, labMap) xs
    f (i, g, labMap) inst@(Label asm lab) =
      let newNode = Node {inst = inst, def = [], use = [], isMove = False} in
      case labMap M.!? lab of
        Nothing   -> ( succ i
                     , ([], i, newNode, [((), succ i)]) & g
                     , M.insert lab i labMap
                     )
        Just node -> ( i
                     , ([], node, newNode, [((), i)]) & (insEdge (i-1, node,()) (delEdge (i-1, i) g))
                     , labMap -- ^ if we already have a node, unset the edge from the previous instruction
                     )
    f (i, g, labMap) inst@(Move asm dst src) =
      let newNode = Node {inst = inst, def = [dst], use = [src], isMove = True} in
      ( succ i
      , ([], i, newNode, [((), succ i)]) & g
      , labMap
      )
