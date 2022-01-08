module Brick.Widgets.FlashMessages where

import Brick (AttrName, EventM, Next, continue)
import Brick.BChan (writeBChan)
import Brick.BChan.Class (HasEventChannel (..))
import Brick.Widgets.FlashMessages.Class
import Brick.Widgets.FlashMessages.Types
import RIO
import qualified RIO.Map as Map

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
      writeBChan (state ^. eventChannelL) $ toFlashMessageEvent $ RemoveFlashMessage currentId
  pure $
    state
      & flashMessagesL %~ Map.insert currentId msg
      & flashMessageIdL %~ (+ 1)

flashSuccessAttr :: AttrName
flashSuccessAttr = "flash-success"

flashErrorAttr :: AttrName
flashErrorAttr = "flash-error"
