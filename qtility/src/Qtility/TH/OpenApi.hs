{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Qtility.TH.OpenApi where

import Data.OpenApi
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import RIO hiding (lift)
import qualified RIO.Char as Char

-- instance ToSchema Board where
--   declareNamedSchema =
--     genericDeclareNamedSchema
--       defaultSchemaOptions {OpenApi.fieldLabelModifier = unlensName "Board"}

lowerCaseFirstCharacter :: String -> String
lowerCaseFirstCharacter [] = []
lowerCaseFirstCharacter (x : xs) = Char.toLower x : xs

prefixedOptions :: String -> (String -> String) -> SchemaOptions
prefixedOptions name' f =
  let fieldLabelModifier = drop (length name' + 1) >>> lowerCaseFirstCharacter >>> f
   in defaultSchemaOptions {fieldLabelModifier}

deriveSchema :: Name -> Q [Dec]
deriveSchema name' = do
  [d|
    instance ToSchema $(conT name') where
      declareNamedSchema =
        genericDeclareNamedSchema $
          prefixedOptions $(lift $ nameBase name') id
    |]

deriveSchema' :: Name -> Name -> Q [Dec]
deriveSchema' optionsName name' = do
  [d|
    instance ToSchema $(conT name') where
      declareNamedSchema =
        genericDeclareNamedSchema $ $(varE optionsName) $(lift $ nameBase name')
    |]
