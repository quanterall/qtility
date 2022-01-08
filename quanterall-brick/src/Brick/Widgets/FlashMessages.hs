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

handleFlashMessageEvent ::
  (HasFlashMessages s, HasEventChannel s event, AsFlashMessageEvent event) =>
  s ->
  FlashMessageEvent ->
  EventM n (Next s)
handleFlashMessageEvent state (AddFlashMessage m) =
  addFlashMessage state m >>= continue
handleFlashMessageEvent state (RemoveFlashMessage i) =
  continue $ state & flashMessagesL %~ Map.delete i

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

flashSuccessAttr :: AttrName
flashSuccessAttr = "flash-success"

flashErrorAttr :: AttrName
flashErrorAttr = "flash-error"
