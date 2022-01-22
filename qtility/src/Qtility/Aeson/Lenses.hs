module Qtility.Aeson.Lenses where

import Control.Lens.Combinators (at)
import Data.Aeson (SumEncoding (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Qtility.Aliases (AesonOptions)
import RIO

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

-- | Prism for reaching into a 'Value' as an 'Object' and accessing a given key.
atKey ::
  (AsValue t, Applicative f) =>
  Text ->
  (Maybe Aeson.Value -> f (Maybe Aeson.Value)) ->
  t ->
  f t
atKey k = _Object . at k
