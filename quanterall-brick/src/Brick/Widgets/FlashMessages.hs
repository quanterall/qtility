module Brick.Widgets.FlashMessages
  ( drawFlashMessages,
    handleFlashMessageEvent,
    addFlashMessage,
    flashSuccessAttr,
    flashErrorAttr,
  )
where

import Brick
import Brick.BChan (writeBChan)
import Brick.BChan.Class (HasEventChannel (..))
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.FlashMessages.Class
import Brick.Widgets.FlashMessages.Types
import Control.Lens ((#))
import RIO
import qualified RIO.Map as Map

-- | Draws a centered vertical box of flash messages. These use 'flashSuccessAttr' and
-- 'flashErrorAttr' for styling.
drawFlashMessages :: (HasFlashMessages s) => s -> Widget n
drawFlashMessages s = centerLayer $ s ^. flashMessagesL & Map.elems & map drawFlashMessage & vBox

drawFlashMessage :: FlashMessage -> Widget n
drawFlashMessage flashMessage = do
  let (flashAttribute, title, body) =
        case flashMessage of
          FlashSuccess b ->
            (flashSuccessAttr, "Success", b)
          FlashError b ->
            (flashErrorAttr, "Error", b)
  withAttr flashAttribute $
    padAll 1 $
      borderWithLabel (txt title) (txt body)

-- | Handles a 'FlashMessageEvent' and produces a new value. The reason we need 'MonadIO' here is
-- because a thread is started in the case where we are adding a flash message, in order for the
-- message to be removed at a later time.
handleFlashMessageEvent ::
  (MonadIO m, HasFlashMessages s, HasEventChannel s event, AsFlashMessageEvent event) =>
  s ->
  FlashMessageEvent ->
  m s
handleFlashMessageEvent state (AddFlashMessage m) =
  addFlashMessage state m
handleFlashMessageEvent state (RemoveFlashMessage i) =
  pure $ state & flashMessagesL %~ Map.delete i

-- | Adds a flash message to your current state, making sure to also start a thread for removing it.
addFlashMessage ::
  (MonadIO m, HasFlashMessages s, HasEventChannel s event, AsFlashMessageEvent event) =>
  s ->
  FlashMessage ->
  m s
addFlashMessage state msg = do
  let currentId = state ^. flashMessageIdL & (+ 1)
  liftIO $
    async $ do
      threadDelay $ 4 * 1000 * 1000
      writeBChan (state ^. eventChannelL) $ _FlashMessageEvent # RemoveFlashMessage currentId
  pure $
    state
      & flashMessagesL %~ Map.insert currentId msg
      & flashMessageIdL %~ (+ 1)

-- | Attribute for success flash messages.
flashSuccessAttr :: AttrName
flashSuccessAttr = "flash-success"

-- | Attribute for error flash messages.
flashErrorAttr :: AttrName
flashErrorAttr = "flash-error"
