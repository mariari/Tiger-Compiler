{-# LANGUAGE TemplateHaskell #-}

module Semantic.Translate
  ( Access
  , Level
  , outerMost
  , newLevel
  , allocLocal
  ) where

import qualified Frame.X86     as F
import qualified Semantic.Temp as T

import Control.Lens hiding(Level)
import Control.Monad.Except

type Escape = Bool

data Level = TopLevel
           | Level { _parent :: Level
                   , _frame  :: F.Frame
                   } deriving Show
makeLenses ''Level

data Access = Access { _level  :: Level
                     , _fAcess :: F.Access
                     } deriving Show
makeLenses ''Access

outerMost :: Level
outerMost = TopLevel

newLevel :: Level -> T.Label -> [Escape] -> IO Level
newLevel parent name formals = do
  frame <- F.newFrame name formals
  return (Level {_parent = parent, _frame = frame})

allocLocal :: (MonadIO m, MonadError String m) => Level -> Bool -> m Access
allocLocal TopLevel _ = throwError ("Tried to allocate on the top level")
allocLocal lvl esc = do
  (fra, access) <- liftIO (F.allocLocal (_frame lvl) esc)
  return (Access (lvl & frame .~ fra) access)

formals :: Level -> [Access]
formals TopLevel = []
formals lvl      = Access lvl <$> F.formals (_frame lvl)
