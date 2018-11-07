module IR.Canonical
  ( linearize
  , basicBlocks
  , traceSchedule
  ) where

import           IR.Tree
import qualified Semantic.Temp as Temp

import qualified Data.Map.Strict as M
import qualified Data.Sequence   as S
import           Data.Sequence(Seq(..))
import           Data.Foldable(toList)

-- | from an arbitrary statement produce a list of cleaned trees that satisfies
-- the following properties
-- 1. no Seq or ESeq
-- 2. the parent of every Call is an Exp or a Move (Temp t)
linearize :: Stmt -> IO [Stmt]
linearize stm = (\s -> linear s []) <$> (doStm stm)

-- | from a cleaned try (lineralized) produce a block with the following properties
-- 1. the properties from lineraize
-- 2. Every blcok begins with a label
-- 3. A label appears only at the beginning of a block
-- 4. any Jump or CJump is the last stm in a block
-- 5. Every block ends iwht a Jump or a CJump
basicBlocks :: [Stmt] -> IO ([Seq Stmt], Temp.Label)
basicBlocks stms = do
  done <- Temp.newLabel
  let blocks (x@(Label {}) : xs) blist = do
        let endBlock stms currBlock = blocks stms (currBlock : blist)

            next :: [Stmt] -> Seq Stmt -> IO [Seq Stmt] -- 2nd arg is the currentBlock
            next (x@(Jump {})  : xs) = endBlock xs . (:|> x)
            next (x@(CJump {}) : xs) = endBlock xs . (:|> x)
            next xs@(Label l   : _)  = next (Jump (Name l) [l] : xs)
            next (x:xs)              = next xs . (:|> x)
            next []                  = next [Jump (Name done) [done]]

        next xs (S.singleton x) -- there is probably a nice way to do this with HOF that I don't see
      blocks [] blist = return (reverse blist)
      blocks stms blist = do
        l <- Temp.newLabel
        blocks (Label l : stms) blist
  blocked <- blocks stms []
  return(blocked, done)

-- | from a block with properties 1-5, along with an exit label
-- produce a list of stms s.t.
-- 1. all properties of basicBlock apply
-- 2. Every CJump _ t f is followed immediately by the f Label
traceSchedule :: ([Seq Stmt], Temp.Label) -> IO [Stmt]
traceSchedule (blocks, done) = toList . (:|> doneL) <$> getNext table blocks
  where
    table = foldr enterBlock mempty blocks
    doneL = Label done
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


-- helper functions for traceSchedule -------------------------------------------------------
enterBlock :: Seq Stmt -> M.Map Temp.Label (Seq Stmt) -> M.Map Temp.Label (Seq Stmt)
enterBlock x@(Label l :<| _) table = M.insert l x table
enterBlock _                 table = table

trace :: M.Map Temp.Label (Seq Stmt) -> Seq Stmt -> [Seq Stmt] -> IO (Seq Stmt)
trace t x@(Label lab :<| _) rest = f x
  where
    table = M.insert lab Empty t -- Mark the label, l, as traced!
    f (most :|> Jump (Name l) _)   = handleJumpName most l
    f (most :|> CJump opr a b t f) = handleCJump most opr a b t f
    f (most :|> Jump {})           = fmap (x <>) (getNext table rest)
    f x                            = error ("f of trace didn't have a jump at the end of the block" <> show x)
    handleJumpName most l =
      case table M.!? l of
        Just x'@(_ :|> _) -> fmap (most <>) (trace table x' rest) -- most removes the Jump label
        _                 -> fmap (x <>)    (getNext table rest)
    handleCJump most opr a b t f =
      case (table M.!?t, table M.!? f) of
        -- handle the false case first to better mask a real instruction jump
        (_, Just x'@(_ :|> _)) -> fmap (x <>) (trace table x' rest)
        (Just x'@(_ :|> _), _) -> fmap ((most :|> CJump (notRel opr) a b f t) <>) (trace table x' rest)
        _  -> do
          f' <- Temp.newLabel
          next <- getNext table rest
          return (most <> S.fromList [ CJump opr a b t f'
                                     , Label f'
                                     , Jump (Name f) [f] ]
                       <> next)

trace _ _ _ = error "trace must take a stmt list without a label first"

getNext :: M.Map Temp.Label (Seq Stmt) -> [Seq Stmt] -> IO (Seq Stmt)
getNext table [] = return Empty
getNext table (x@(Label l :<| _) : xs) = case table M.!? l of
  Just (_ :|> _) -> trace table x xs
  _              -> getNext table xs
getNext _ _ = error "getNext was given a stmt list without a label first"
