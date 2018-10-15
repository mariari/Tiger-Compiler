module IR.Canonical
  ( linearize
  , basicBlocks
  , traceSchedule
  ) where


import           IR.Tree
import qualified Semantic.Temp as Temp

-- | removes ESeqs and moves Call to the top Level
linearize :: Stmt -> [Stmt]
linearize = undefined

basicBlocks :: [Stmt] -> ([[Stmt]], Temp.Label)
basicBlocks = undefined

traceSchedule :: [[Stmt]] -> Temp.Label -> [Stmt]
traceSchedule = undefined

-- Helper functions --------------------------------------------------------------------------------

-- | given a Stmt and an Exp, check if they *definitely* commute
commute :: Stmt -> Exp -> Bool
commute (Exp (Const _)) _ = True
commute _ (Name _)        = True
commute _ (Const _)       = True
commute _ _               = False
