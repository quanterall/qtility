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
import RIO as Export hiding (fromEither, fromEitherM, id, map)
import qualified RIO

type AesonOptions = Aeson.Options

defaultAesonOptions :: AesonOptions
defaultAesonOptions = Aeson.defaultOptions

fieldLabelModifier :: AesonOptions -> String -> String
fieldLabelModifier = Aeson.fieldLabelModifier

constructorTagModifier :: AesonOptions -> String -> String
constructorTagModifier = Aeson.constructorTagModifier

allNullaryToStringTag :: AesonOptions -> Bool
allNullaryToStringTag = Aeson.allNullaryToStringTag

omitNothingFields :: AesonOptions -> Bool
omitNothingFields = Aeson.omitNothingFields

sumEncoding :: AesonOptions -> SumEncoding
sumEncoding = Aeson.sumEncoding

unwrapUnaryRecords :: AesonOptions -> Bool
unwrapUnaryRecords = Aeson.unwrapUnaryRecords

tagSingleConstructors :: AesonOptions -> Bool
tagSingleConstructors = Aeson.tagSingleConstructors

rejectUnknownFields :: AesonOptions -> Bool
rejectUnknownFields = Aeson.rejectUnknownFields

identity :: a -> a
identity = RIO.id

map :: (Functor f) => (a -> b) -> f a -> f b
map = RIO.fmap
