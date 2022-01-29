-- | Utilities for taking structures like `Maybe` & `Either` and turning them into exceptions, as
-- well as utilities for dealing with exceptions with the help of prisms.
module Qtility.Exceptions where

import Control.Lens ((#))
import Control.Lens.Prism (Prism')
import Qtility.Data (note)
import RIO hiding (fromEither, fromEitherM)

-- | Takes a prism and an action to run. If the action fails with an exception matching the prism,
-- the exception is caught and wrapped up in an 'Either'. Notably this does not narrow the type of
-- the exception like 'Control.Exception.Lens.catching' does.
tryAs :: (MonadThrow m, MonadUnliftIO m, Exception e) => Prism' e e' -> m a -> m (Either e a)
tryAs p m = catchJust (^? p) (Right <$> m) ((p #) >>> Left >>> pure)

-- | Takes a @'Prism' e e'@ and maps a @'Left' e'@ to a @'Left' e@. This widens the error type into
-- what is probably idiomatically an `AsError` class, often created with
-- 'Control.Lens.TH.makeClassyPrisms'.
mapLeftAs :: Prism' e e' -> Either e' a -> Either e a
mapLeftAs p = mapLeft (p #)

-- | Throws an exception from an @'Either' e a@ where @e@ is an 'Exception'.
fromEither :: (Exception e, MonadThrow m) => Either e a -> m a
fromEither = either throwM pure

-- | Throws an exception from an @m ('Either' e a)@ where @e@ is an 'Exception'.
fromEitherM :: (Exception e, MonadThrow m) => m (Either e a) -> m a
fromEitherM ma = ma >>= fromEither

-- | Takes a monadic action producing a @m 'Maybe'@ and throws a given exception @e@ from it. This
-- is just a specialization of @'fromEitherM' '$' 'note' e '<$>' action@.
fromMaybeM :: (Exception e, MonadThrow m) => e -> m (Maybe a) -> m a
fromMaybeM e = fmap (note e) >>> fromEitherM

-- | Takes a @Maybe a@ and if it is 'Nothing', throws a given 'Exception' @e@.
fromPureMaybeM :: (Exception e, MonadThrow m) => e -> Maybe a -> m a
fromPureMaybeM e = note e >>> fromEither
