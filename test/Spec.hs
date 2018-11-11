import Semantic.Analysis
import TigerParser
import App.Initialize
import App.Environment
import Semantic.Environment
import Data.IORef
import Data.Graph.Inductive.Graph

import Generation.Assembly
import Semantic.Temp
import Liveness.Flow

--main :: IO ()
main = test1 "./test/queens.tig"

test1 str = do
  Right x <- parseTigerFile str
  env   <- genEnv
  baseE <- baseEmap
  exp   <- transExp baseTmap baseE x env
  return (env,exp)



testGraphgen = do
  a <- newTemp
  b <- newTemp
  c <- newTemp
  l1 <- newLabel
  l2 <- newLabel
  let insts = [ Oper "a := 0" [a] [] Nothing
              , Label "L1" l1
              , Move "b := a + 1" b a
              , Oper "c := c + b" [c] [c,b] Nothing
              , Move "a := b * 2" a b
              , Oper "a < N"      []  [a]   (Just [l1, l2])
              , Label "L2" l2
              , Oper "return c"   [] [c] Nothing
              ]
  prettyPrint (instsToGrph insts)
