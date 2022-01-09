module Qtility.Standard
  ( module Export,
    AesonOptions,
    defaultAesonOptions,
    fieldLabelModifier,
    constructorTagModifier,
    omitNothingFields,
    sumEncoding,
    unwrapUnaryRecords,
    tagSingleConstructors,
    rejectUnknownFields,
    allNullaryToStringTag,
    identity,
  )
where

import Control.Concurrent.STM.TBMChan as Export
import Control.Concurrent.STM.TBMQueue as Export
import Control.Lens as Export (re, (#))
import Control.Lens.Prism as Export
import Control.Lens.TH as Export (makeClassy, makeClassyPrisms, makeLenses, makeWrapped)
import Data.Aeson as Export
  ( FromJSON (..),
    SumEncoding (..),
    ToJSON (..),
    defaultTaggedObject,
    eitherDecode,
    eitherDecode',
    eitherDecodeStrict,
    eitherDecodeStrict',
    encode,
    genericParseJSON,
    genericToJSON,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.Aeson as Aeson
import Qtility.Data as Export
import Qtility.Environment as Export
import Qtility.Environment.Types as Export
import Qtility.TH as Export
import Qtility.TH.JSON as Export
import RIO as Export hiding (fromEither, fromEitherM)
import qualified RIO

-- | A type alias for 'Aeson.Options'. 'Aeson.Options' is unfortunately named and will often clash
-- with project-specific types called @Options@, so with this we can use 'AesonOptions' to
-- disambiguate the situation. For a 'Aeson.defaultOptions' alias, see 'defaultAesonOptions'.
type AesonOptions = Aeson.Options

-- | An alias for 'Aeson.defaultOptions'. See documentation for 'AesonOptions' for more information
-- on purpose.
defaultAesonOptions :: AesonOptions
defaultAesonOptions = Aeson.defaultOptions

fieldLabelModifier :: Lens' AesonOptions (String -> String)
fieldLabelModifier = lens Aeson.fieldLabelModifier (\o f -> o {Aeson.fieldLabelModifier = f})

constructorTagModifier :: Lens' AesonOptions (String -> String)
constructorTagModifier =
  lens Aeson.constructorTagModifier (\o f -> o {Aeson.constructorTagModifier = f})

allNullaryToStringTag :: Lens' AesonOptions Bool
allNullaryToStringTag =
  lens Aeson.allNullaryToStringTag (\o f -> o {Aeson.allNullaryToStringTag = f})

omitNothingFields :: Lens' AesonOptions Bool
omitNothingFields = lens Aeson.omitNothingFields (\o f -> o {Aeson.omitNothingFields = f})

sumEncoding :: Lens' AesonOptions SumEncoding
sumEncoding = lens Aeson.sumEncoding (\o f -> o {Aeson.sumEncoding = f})

unwrapUnaryRecords :: Lens' AesonOptions Bool
unwrapUnaryRecords = lens Aeson.unwrapUnaryRecords (\o f -> o {Aeson.unwrapUnaryRecords = f})

tagSingleConstructors :: Lens' AesonOptions Bool
tagSingleConstructors =
  lens Aeson.tagSingleConstructors (\o f -> o {Aeson.tagSingleConstructors = f})

rejectUnknownFields :: Lens' AesonOptions Bool
rejectUnknownFields = lens Aeson.rejectUnknownFields (\o f -> o {Aeson.rejectUnknownFields = f})

-- | An alias for `id`, so we can disambiguate in cases where `id` could mean several things.
identity :: a -> a
identity = RIO.id
