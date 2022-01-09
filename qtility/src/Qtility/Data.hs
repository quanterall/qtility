module Qtility.Data where

import Control.Lens ((#))
import Control.Lens.Prism (Prism', prism')
import Control.Lens.Wrapped (Unwrapped, Wrapped, _Unwrapped', _Wrapped')
import RIO hiding (fromEither, fromEitherM)
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

-- | 'Prism'' for reading a value from 'Text'
fromText :: (Read a, Show a) => Prism' Text a
fromText = prism' tshow tReadMaybe

-- | Takes a prism and an action to run. If the action fails with an exception matching the prism,
-- the exception is caught and wrapped up in an 'Either'. Notably this does not narrow the type of
-- the exception like 'Control.Exception.Lens.catching' does.
tryAs :: (MonadThrow m, MonadUnliftIO m, Exception e) => Prism' e e' -> m a -> m (Either e a)
tryAs p m = catchJust (^? p) (Right <$> m) ((p #) >>> Left >>> pure)

-- | Takes a @'Prism' e e'@ and maps a `Left e'` to a `Left e`. This widens the error type into what
-- is probably idiomatically an `AsError` class.
mapLeftAs :: Prism' e e' -> Either e' a -> Either e a
mapLeftAs p = mapLeft (p #)

-- | Throws an exception from an @Either e a@ where @e@ is an 'Exception'.
fromEither :: (Exception e, MonadThrow m) => Either e a -> m a
fromEither = either throwM pure

-- | Throws an exception from an @m (Either e a)@ where @e@ is an 'Exception'.
fromEitherM :: (Exception e, MonadThrow m) => m (Either e a) -> m a
fromEitherM ma = ma >>= fromEither

-- | Takes a monadic action producing a @m Maybe@ and throws a given exception @e@ from it. This
-- is just a specialization of @'fromEitherM' '$' note e '<$>' action@.
fromMaybeM :: (Exception e, MonadThrow m) => e -> m (Maybe a) -> m a
fromMaybeM e = fmap (note e) >>> fromEitherM

-- | Takes a @Maybe a@ and if it is 'Nothing', throws a given 'Exception' @e@.
fromPureMaybeM :: (Exception e, MonadThrow m) => e -> Maybe a -> m a
fromPureMaybeM e = note e >>> fromEither

-- | Lifted version of 'Data.List.find'.
findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _p [] = pure Nothing
findM p (a : as) = do
  result <- p a
  if result then pure (Just a) else findM p as

-- | Convenience function for `_Wrapped` that seems to capture meaning better. Unwraps something
-- that wraps another type safely, i.e. it can actually be coerced safely into the other type. This
-- is a very useful lens to have for automatically reaching into a newtype without specifying
-- the name exactly.
--
-- @
--     newtype PortNumber = PortNumber {_unPortNumber :: Int}
--       deriving (Eq, Show, Generic)
--
--     makeWrapped ''PortNumber
-- @
--
-- With the above defined and assuming @p@ wraps a numeric, the following will automatically apply
-- @+ 1@ to the value inside the structure, and automatically keep it wrapped:
--
-- @
--     p & unwrap %~ (+ 1)
-- @
unwrap :: (Wrapped a) => Lens' a (Unwrapped a)
unwrap = _Wrapped'

wrap :: (Wrapped a) => Lens' (Unwrapped a) a
wrap = _Unwrapped'
