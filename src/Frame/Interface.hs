module Frame.Interface where

import Semantic.Temp as Temp

type Escape = Bool

-- NOTE:: this code does not have all the interfaces values
-- Ideally we also want a Registers type that holds all our registers
-- this will go in the Env type in App.Environment

class FrameFn frame where
  newFrame :: Temp.Label -> [Escape] -> IO frame
  name     :: frame -> Temp.Label

class FrameFn frame => FrameInter frame access where
  formals    :: frame -> [access]
  allocLocal :: frame -> Escape -> IO (frame, access)
