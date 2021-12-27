module Qtility.Data where

import RIO
import qualified RIO.Text as Text

-- | Annotates what would be a 'Nothing' with an error, taking it into the domain of 'Either'.
note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

-- | Silences an 'Either', taking it into the domain of 'Maybe'.
hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

-- | 'readMaybe' but for 'Text'.
tReadMaybe :: (Read a) => Text -> Maybe a
tReadMaybe = Text.unpack >>> readMaybe
