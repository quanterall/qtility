module Brick.Prism where

import Brick (BrickEvent (..))
import Control.Lens.Prism
import RIO

_AppEvent :: Prism' (BrickEvent n e) e
_AppEvent = prism' AppEvent $ \case
  AppEvent e -> Just e
  _ -> Nothing
