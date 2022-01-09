module Qtility.Aliases where

import qualified Data.Aeson as Aeson
import qualified RIO

-- | A type alias for 'Aeson.Options'. 'Aeson.Options' is unfortunately named and will often clash
-- with project-specific types called @Options@, so with this we can use 'AesonOptions' to
-- disambiguate the situation. For a 'Aeson.defaultOptions' alias, see 'defaultAesonOptions'.
type AesonOptions = Aeson.Options

-- | An alias for 'Aeson.defaultOptions'. See documentation for 'AesonOptions' for more information
-- on purpose.
defaultAesonOptions :: AesonOptions
defaultAesonOptions = Aeson.defaultOptions

-- | An alias for `id`, so we can disambiguate in cases where `id` could mean several things.
identity :: a -> a
identity = RIO.id
