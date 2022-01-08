{-# LANGUAGE TemplateHaskell #-}

module Brick.Widgets.FlashMessages.Types where

import Control.Lens.TH (makeClassyPrisms)
import RIO

-- | Flash message content, a message meant to inform the user of an application of a recent event
-- either succeeding or failing, or otherwise interesting information.
data FlashMessage
  = FlashSuccess !Text
  | FlashError !Text
  deriving (Eq, Show)

-- | Represents an event to modify the application's flash message state.
data FlashMessageEvent
  = -- | Removes a flash message, addressed by its index/id.
    RemoveFlashMessage !Int
  | -- | Adds a flash message.
    AddFlashMessage !FlashMessage
  deriving (Eq, Show)

foldMapM makeClassyPrisms [''FlashMessage]
