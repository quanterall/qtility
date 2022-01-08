-- | Class module for use when defining 'HasEventChannel' instances.
module Brick.BChan.Class where

import Brick.BChan (BChan)
import RIO

-- | Environments with a channel to which we can send events of type @event@.
class HasEventChannel env event | env -> event where
  eventChannelL :: Lens' env (BChan event)
