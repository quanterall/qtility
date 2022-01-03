module Qtility.Standard
  ( module Export,
    AesonOptions,
    defaultAesonOptions,
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
  )
import qualified Data.Aeson as Aeson
import Qtility.Data as Export
import Qtility.Environment as Export
import Qtility.Environment.Types as Export
import Qtility.TH.JSON as Export
import Qtility.TH.Lens as Export
import RIO as Export hiding (fromEither, fromEitherM)

type AesonOptions = Aeson.Options

defaultAesonOptions :: AesonOptions
defaultAesonOptions = Aeson.defaultOptions
