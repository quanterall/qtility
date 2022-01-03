module Qtility.Standard
  ( module Export,
    AesonOptions,
    defaultAesonOptions,
    identity,
    map,
  )
where

import Data.Aeson as Export
  ( FromJSON (..),
    ToJSON (..),
    eitherDecode,
    eitherDecode',
    eitherDecodeStrict,
    eitherDecodeStrict',
    encode,
    genericParseJSON,
    genericToJSON,
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

identity :: a -> a
identity = RIO.id

map :: (Functor f) => (a -> b) -> f a -> f b
map = RIO.fmap
