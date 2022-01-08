module Brick.Widgets.FlashMessages.Class where

import Brick.Widgets.FlashMessages.Types
import Control.Lens.Prism (Prism')
import RIO

class HasFlashMessages env where
  flashMessagesL :: Lens' env (Map Int FlashMessage)
  flashMessageIdL :: Lens' env Int

class AsFlashMessageEvent event where
  _FlashMessageEvent :: Prism' event FlashMessageEvent
