module Qtility.Data where

import Control.Lens.Combinators (Cons, cons, uncons)
import Control.Lens.Prism (Prism', prism')
import Control.Lens.Wrapped (Unwrapped, Wrapped, _Unwrapped', _Wrapped')
import RIO hiding (fromEither, fromEitherM)
import qualified RIO.Char as Char
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

-- | Lifted version of 'Data.List.find'.
findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _p [] = pure Nothing
findM p (a : as) = do
  result <- p a
  if result then pure (Just a) else findM p as

-- | Convenience function for '_Wrapped' that seems to capture meaning better. Unwraps something
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

-- | Takes a default left value and a list of @'Either' l r@ values and returns either the first
-- @'Right' a@ value or the default left value if no 'Right' can be found.
firstRight :: l -> [Either l r] -> Either l r
firstRight l = foldr f (Left l)
  where
    f (Right r) _ = Right r
    f _ eithers = eithers

-- | Uppercases the first character in a collection of characters.
upperCaseFirst :: (Cons s s Char Char) => s -> s
upperCaseFirst t =
  case uncons t of
    Just (c, rest) -> cons (Char.toUpper c) rest
    Nothing -> t

-- | Lowercases the first character in a collection of characters.
lowerCaseFirst :: (Cons s s Char Char) => s -> s
lowerCaseFirst t =
  case uncons t of
    Just (c, rest) -> cons (Char.toLower c) rest
    Nothing -> t
