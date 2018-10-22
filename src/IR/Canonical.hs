module IR.Canonical
  ( linearize
  , basicBlocks
  , traceSchedule
  ) where


import           IR.Tree
import qualified Semantic.Temp as Temp

-- | removes ESeqs and moves Call to the top Level
linearize :: Stmt -> IO [Stmt]
linearize stm = (\s -> linear s []) <$> (doStm stm)

traceSchedule :: [[Stmt]] -> Temp.Label -> [Stmt]
traceSchedule = undefined

basicBlocks :: [Stmt] -> IO ([[Stmt]], Temp.Label)
basicBlocks stms = do
  done <- Temp.newLabel
  let blocks (x@(Label {}) : xs) blist = do
        let endBlock stms currBlock = blocks stms (reverse currBlock : blist)

            next :: [Stmt] -> [Stmt] -> IO [[Stmt]] -- 2nd arg is the currentBlock
            next (x@(Jump {})  : xs) = endBlock xs . (x :)
            next (x@(CJump {}) : xs) = endBlock xs . (x :)
            next xs@(Label l   : _)  = next (Jump (Name l) [l] : xs)
            next (x:xs)              = next xs . (x :)
            next []                  = next [Jump (Name done) [done]]

        next xs [x] -- there is probably a nice way to do this with HOF that I don't see
      blocks [] blist = return (reverse blist)
      blocks stms blist = do
        l <- Temp.newLabel
        blocks (Label l : stms) blist
  blocked <- blocks stms []
  return(blocked, done)
-- Helper functions --------------------------------------------------------------------------------

-- | given a Stmt and an Exp, check if they *definitely* commute
commute :: Stmt -> Exp -> Bool
commute (Exp (Const _)) _ = True
commute _ (Name _)        = True
commute _ (Const _)       = True
commute _ _               = False

nop = Exp (Const 0)

combStm :: Stmt -> Stmt -> Stmt
combStm (Exp (Const _)) x = x
combStm x (Exp (Const _)) = x
combStm x y               = Seq x y

combStms :: Foldable t => t Stmt -> Stmt
combStms = foldr1 combStm

reorder :: [Exp] -> IO (Stmt, [Exp])
reorder [] = return (nop, [])
reorder (x@(Call {}) : xs) = do
  t <- Temp.newTemp
  reorder (ESeq (Move (Temp t) x) (Temp t) : xs)
reorder (x:xs) = do
  (stmsX,  y)  <- doExp x
  (stmsXs, ys) <- reorder xs
  if commute stmsXs y then
      return (combStm stmsX stmsXs, y : ys)
    else do
      t <- Temp.newTemp
      return (combStms [stmsX, Move (Temp t) y, stmsXs], Temp t : ys)

reorderExp :: [Exp] -> ([Exp] -> b) -> IO (Stmt, b)
reorderExp el build = do
  (stms, el') <- reorder el
  return (stms, build el')

reorderStm :: [Exp] -> ([Exp] -> Stmt) -> IO Stmt
reorderStm el build = do
  (stms, el') <- reorder el
  return (combStm stms (build el'))

doStm :: Stmt -> IO Stmt
doStm (Seq x y)                   = combStm <$> doStm x <*> doStm y
doStm (Exp (Call e el))           = reorderStm (e:el) (\(e:el) -> Exp (Call e el))
doStm (Exp x)                     = reorderStm [x]    (\[x]    -> Exp x)
doStm (Jump x ls)                 = reorderStm [x]    (\[x]    -> Jump x ls)
doStm (CJump p x y t f)           = reorderStm [x,y]  (\[x,y]  -> CJump p x y t f)
doStm (Move (Temp t) (Call e el)) = reorderStm (e:el) (\(e:el) -> Move (Temp t) (Call e el))
doStm (Move (Temp t) x)           = reorderStm [x]    (\[x]    -> Move (Temp t) x)
doStm (Move (Mem t)  x)           = reorderStm [t,x]  (\[t,x]  -> Move (Mem t) x)
doStm (Move (ESeq s e) x)         = doStm (Seq s (Move e x))
doStm s                           = reorderStm [] (\[] -> s)

doExp :: Exp -> IO (Stmt, Exp)
doExp (Binop x p y) = reorderExp [x,y]  (\[x,y]  -> Binop x p y)
doExp (Neg x)       = reorderExp [x]    (\[x]    -> Neg x)
doExp (Mem x)       = reorderExp [x]    (\[x]    -> Mem x)
doExp (Call e el)   = reorderExp (e:el) (\(e:el) -> Call e el)
doExp (ESeq s e)    = (\s (s',x) -> (combStm s s', x)) <$> doStm s <*> doExp e
doExp e             = reorderExp [] (\[] -> e)


linear :: Stmt -> [Stmt] -> [Stmt]
linear (Seq x y) = linear x . linear y
linear x         = (x :)
