module Frame.Interface where

import Semantic.Temp as Temp

type Escape = Bool

class FrameFn frame where
  newFrame :: Temp.Label -> [Escape] -> IO frame
  name     :: frame -> Temp.Label

class FrameFn frame => Frame frame access where
  formals    :: frame -> [access]
  allocLocal :: frame -> Escape -> IO (frame, access)
