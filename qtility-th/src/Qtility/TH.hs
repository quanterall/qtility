module Qtility.TH where

import Control.Lens.TH (makeLenses)
import Language.Haskell.TH
import Qtility.TH.JSON (deriveJSON)
import RIO

-- | Derives both lens definitions as well as both 'ToJSON' and 'FromJSON'. The type is assumed to
-- follow the format that 'deriveJSON' requires, where all fields are prefixed with an underscore as
-- well as the name of the type:
--
-- @
--     data Foo = Foo { _fooBar :: Int, _fooBaz :: String }
-- @
--
-- This will generate @fooBar@ and @fooBaz@ lenses and 'ToJSON' and 'FromJSON' instances for the
-- structure without the prefix at all, i.e. @bar@ and @baz@ fields.
deriveLensAndJSON :: Name -> Q [Dec]
deriveLensAndJSON name = do
  lenses <- makeLenses name
  (lenses <>) <$> deriveJSON name
