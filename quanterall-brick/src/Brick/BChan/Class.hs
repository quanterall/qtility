module Brick.BChan.Class where

import Brick.BChan (BChan)
import RIO

class HasEventChannel env event | env -> event where
  eventChannelL :: Lens' env (BChan event)
