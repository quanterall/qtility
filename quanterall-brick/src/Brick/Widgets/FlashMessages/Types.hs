module Brick.Widgets.FlashMessages.Types where

import RIO

data FlashMessage
  = FlashSuccess !Text
  | FlashError !Text
  deriving (Eq, Show)

data FlashMessageEvent
  = RemoveFlashMessage !Int
  | AddFlashMessage !FlashMessage
  deriving (Eq, Show)
