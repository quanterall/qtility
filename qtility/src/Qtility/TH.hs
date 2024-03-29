module Qtility.TH
  ( deriveLensAndJSON,
    deriveLensAndAbbreviatedJSON,
    deriveLensAndJSON',
    deriveClassyLensAndJSON,
  )
where

import Control.Lens.TH (makeClassy, makeLenses)
import Language.Haskell.TH
import Qtility.TH.JSON (deriveAbbreviatedJSON, deriveJSON, deriveJSON')
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

-- | Like 'deriveLensAndJSON' but takes the name of an 'Data.Aeson.Options' object for how to
-- decode/encode JSON.
deriveLensAndJSON' :: Name -> Name -> Q [Dec]
deriveLensAndJSON' optionsName name = do
  lenses <- makeLenses name
  (lenses <>) <$> deriveJSON' optionsName name

-- | Like 'deriveLensAndJSON' but specialized for types with abbreviated lens prefixes. For example:
--
-- @
--    data SetOfPossibleValues = SetOfPossibleValues
--      { _sopvName :: Text,
--        _sopvValues :: [Text]
--      }
--      deriving (Eq, Show, Generic)
--
--    deriveAbbreviatedLensAndJSON ''SetOfPossibleValues
-- @
--
-- This will generate the appropriate lenses and also make sure that your 'ToJSON' and 'FromJSON'
-- instances are correctly created with field label modifiers that remove the `_sopv` prefix.
deriveLensAndAbbreviatedJSON :: Name -> Q [Dec]
deriveLensAndAbbreviatedJSON name = do
  lenses <- makeLenses name
  (lenses <>) <$> deriveAbbreviatedJSON name

-- | Derives both classy lens definitions as well as both 'ToJSON' and 'FromJSON'. The type is
-- assumed to follow the format that 'deriveJSON' requires, where all fields are prefixed with an
-- underscore as well as the name of the type:
--
-- @
--     data Foo = Foo { _fooBar :: Int, _fooBaz :: String }
-- @
--
-- This generates a @HasFoo@ class with @fooBar@ and @fooBaz@ lenses and 'ToJSON' and 'FromJSON'
-- instances for the structure without the prefix at all, i.e. @bar@ and @baz@ fields.
deriveClassyLensAndJSON :: Name -> Q [Dec]
deriveClassyLensAndJSON name = do
  lenses <- makeClassy name
  (lenses <>) <$> deriveJSON name
