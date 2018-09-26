{-# LANGUAGE AllowAmbiguousTypes #-}

module Frame.Interface where

import Semantic.Temp as Temp

type Escape = Bool

class Frame frame access where
  newFrame   :: Temp.Label -> [Escape] -> frame
  name       :: frame -> Temp.Label
  formals    :: frame -> [access]
  allocLocal :: frame -> Escape -> IO (frame, access)
