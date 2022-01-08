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
    map,
  )
where

import Control.Lens.Prism as Export
import Control.Lens.TH as Export (makeClassyPrisms, makeLenses)
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
import Qtility.TH.Lens as Export
import RIO as Export hiding (fromEither, fromEitherM, map)
import qualified RIO

type AesonOptions = Aeson.Options

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

identity :: a -> a
identity = RIO.id

map :: (Functor f) => (a -> b) -> f a -> f b
map = RIO.fmap
