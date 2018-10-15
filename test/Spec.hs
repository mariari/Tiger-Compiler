import Semantic.Analysis
import TigerParser
import App.Initialize
import App.Environment
import Semantic.Environment
import Data.IORef

--main :: IO ()
main = test1 "./test/queens.tig"

test1 str = do
  Right x <- parseTigerFile str
  env   <- genEnv
  baseE <- baseEmap
  exp   <- transExp baseTmap baseE x env
  return (env,exp)
