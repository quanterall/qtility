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

-- | Takes a monadic action producing a @m Maybe@ and throws a given exception @e@ from it. This
-- is just a specialization of @'fromEitherM' '$' note e '<$>' action@.
fromMaybeM :: (Exception e, MonadIO m) => e -> m (Maybe a) -> m a
fromMaybeM e = fmap (note e) >>> fromEitherM

-- | Lifted version of 'Data.List.find'.
findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _p [] = pure Nothing
findM p (a : as) = do
  result <- p a
  if result then pure (Just a) else findM p as
