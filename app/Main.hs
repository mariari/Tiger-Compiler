module Main where

import Semantic.Analysis
import TigerParser
import App.Initialize
import App.Environment
import Semantic.Environment
import Semantic.Escape
import Data.IORef

main :: IO ()
main = test1 "./test/merge.tig" >>= print

test1 :: FilePath -> IO (Env, Expty)
test1 str = do
  Right x <- parseTigerFile str
  traverseExp emptyMap x
  env   <- genEnv
  baseE <- baseEmap
  exp   <- transExp baseTmap baseE x env
  return (env,exp)
