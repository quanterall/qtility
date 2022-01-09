-- | Class module for making an application's environment and events work with flash messages.
module Brick.Widgets.FlashMessages.Class where

import Brick.Widgets.FlashMessages.Types
import Control.Lens.Prism (Prism')
import RIO

-- | Environments that have flash message state.
class HasFlashMessages env where
  flashMessagesL :: Lens' env (Map Int FlashMessage)
  flashMessageIdL :: Lens' env Int

-- | Types that can be interpreted as flash message events.
class AsFlashMessageEvent event where
  _FlashMessageEvent :: Prism' event FlashMessageEvent
