module Main where

import Semantic.Analysis
import TigerParser
import App.Initialize
import App.Environment
import Semantic.Environment
import Semantic.Escape
import Semantic.Translate
import IR.Canonical
import Data.IORef

main :: IO ()
main = test1 "./test/merge.tig" >>= print

test1 :: FilePath -> IO (Env, Expty)
test1 str = do
  Right x <- parseTigerFile str
  traverseExp (mempty,0) x
  env   <- genEnv
  baseE <- baseEmap
  exp   <- transExp baseTmap baseE x env
  return (env,exp)


test2 = do
  (env,Expty {_expr = exp}) <- test1 "./test/queens.tig"
  x <- unNx exp
  linearize x >>= basicBlocks >>= traceSchedule
