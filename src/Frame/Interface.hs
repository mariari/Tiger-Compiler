{-# LANGUAGE AllowAmbiguousTypes #-}
module Frame.Interface where

import Semantic.Temp as Temp

class Frame frame access where
  newFrame   :: Temp.Label -> [Bool] -> frame
  name       :: frame -> Temp.Label
  formals    :: frame -> [access]
  allocLocal :: frame -> Bool -> access
