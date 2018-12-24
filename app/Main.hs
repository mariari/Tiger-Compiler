module Main where

import TigerParser
import Semantic.Analysis
import App.Initialize
import App.Environment
import Semantic.Environment
import Semantic.Escape
import Semantic.Fragment
import Semantic.Temp(Temp)
import IR.Canonical
import qualified Allocation.RegAlloc  as RegAlloc
import qualified Frame.CurrentMachine as Frame
import qualified Generation.X86Gen    as X86
import qualified Generation.Assembly  as Assembly

import Data.Foldable(traverse_)
import Data.Maybe(fromMaybe)
import Data.HashMap.Strict as M
import Control.Monad.Reader hiding (sequence)
import Data.IORef
import System.IO

main :: IO ()
main = test2 >> return ()

tempName :: HasRegs s Frame.Registers => HashMap Temp Frame.Register -> s -> Temp -> String
tempName alloc env temp = Frame.sayName (fromMaybe temp (M.lookup temp alloc)) env

emitproc out (Str labl str) = liftIO $ appendFile out (Frame.string labl str <> "\n")
emitproc out (Proc {body = body, f = frame}) = do
  stmts            <- liftIO $ linearize body >>= basicBlocks >>= traceSchedule
  instrs           <- concat <$> traverse (X86.codegen frame) stmts
  instrs2          <- Frame.procEntryExit2 frame instrs
  (instrs3, alloc) <- RegAlloc.alloc instrs2 frame
  env              <- ask
  let formatInst    = Assembly.format (tempName alloc env)
  -- add call to procEntryExit3, will give prologue, body, and epilogue
  -- debugging information for now
  liftIO $ do
    print "statements: "
    traverse print stmts
    print "instructions: "
    traverse print instrs
    print "alloc: "
    print alloc
    print "regmap: "
    print (Frame.regmap env)
    traverse_ (appendFile out . formatInst) instrs3

test1 :: FilePath -> IO (Env, [Frag])
test1 str = do
  Right x <- parseTigerFile str
  escape x
  env   <- genEnv -- keep this around after running exp
  baseE <- baseEmap
  frags <- transExp baseTmap baseE x env
  print x
  print frags
  return (env,frags)

test2 = do
  (env,frags) <- test1 "./test/queens.tig"
  traverse (\frag -> runReaderT (emitproc "./compiled.asm" frag) env) frags
