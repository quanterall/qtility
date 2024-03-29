{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Qtility.TH.JSON
  ( deriveJSON,
    deriveJSON',
    deriveJSONs,
    deriveJSONs',
    prefixedLensOptions,
    prefixedAbbreviatedLensOptions,
    deriveAbbreviatedJSON,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import Qtility.Data (lowerCaseFirst)
import RIO hiding (lift)
import qualified RIO.Char as Char

-- | An easy way to generate a 'Options' object from a name and a function. This assumes that your
-- fields are prefixed with an underscore as well as the name of the type.
prefixedLensOptions :: String -> (String -> String) -> Options
prefixedLensOptions name f =
  let fieldLabelModifier = drop (length name + 1) >>> lowerCaseFirst >>> f
   in defaultOptions {fieldLabelModifier}

prefixedAbbreviatedLensOptions :: String -> (String -> String) -> Options
prefixedAbbreviatedLensOptions name f =
  let nameAbbreviationLength = name & filter Char.isUpper & length
      fieldLabelModifier = drop (nameAbbreviationLength + 1) >>> lowerCaseFirst >>> f
   in defaultOptions {fieldLabelModifier}

-- | Generates standard 'ToJSON' and 'FromJSON' instances based on the format that the fields in a
-- type are prefixed both with underscores as well as the name of the type that they are defined
-- for. An example would be a type called @Import@ that has two fields, @name@ and @version@:
--
-- @
--    data Import = Import {_importName :: String, _importVersion :: String}
-- @
deriveJSON :: Name -> Q [Dec]
deriveJSON name = do
  [d|
    instance FromJSON $(conT name) where
      parseJSON = genericParseJSON $ prefixedLensOptions $(lift $ nameBase name) id

    instance ToJSON $(conT name) where
      toJSON = genericToJSON $ prefixedLensOptions $(lift $ nameBase name) id
    |]

-- | Generates JSON instances and allows one to pass the name of a function that takes the type name
-- and creates an 'Options' object from it. This allows more flexibility when needed.
deriveJSON' :: Name -> Name -> Q [Dec]
deriveJSON' optionsName name = do
  [d|
    instance FromJSON $(conT name) where
      parseJSON = genericParseJSON $ $(varE optionsName) $(lift $ nameBase name)

    instance ToJSON $(conT name) where
      toJSON = genericToJSON $ $(varE optionsName) $(lift $ nameBase name)
    |]

-- | Generates standard 'ToJSON' and 'FromJSON' instances for fields that are using lensed prefixes
-- with abbreviation for the type name. An example would be a type called @SetOfPossibleValues@:
--
-- @
--   data SetOfPossibleValues = SetOfPossibleValues {_sopvName :: String, _sopvValues :: [String]}
--     deriving (Eq, Show, Generic)
-- @
--
-- This will make sure that your 'ToJSON' and 'FromJSON' instances are correctly created with
-- field label modifiers that remove only the abbreviation, i.e. `_sopv` in this example.
deriveAbbreviatedJSON :: Name -> Q [Dec]
deriveAbbreviatedJSON name = do
  [d|
    instance FromJSON $(conT name) where
      parseJSON = genericParseJSON $ prefixedAbbreviatedLensOptions $(lift $ nameBase name) id

    instance ToJSON $(conT name) where
      toJSON = genericToJSON $ prefixedAbbreviatedLensOptions $(lift $ nameBase name) id
    |]

-- | Takes several names and generates standard 'ToJSON' and 'FromJSON' instances for each of them.
deriveJSONs :: [Name] -> Q [Dec]
deriveJSONs = foldMapM deriveJSON

-- | Takes several names and generates standard 'ToJSON' and 'FromJSON' instances for each of them.
-- Also allows one to pass the name of a function that will decide what 'Options' object to use.
deriveJSONs' :: Name -> [Name] -> Q [Dec]
deriveJSONs' optionsName = foldMapM (deriveJSON' optionsName)
