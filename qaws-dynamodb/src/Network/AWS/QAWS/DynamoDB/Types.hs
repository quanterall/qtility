{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.DynamoDB.Types where

import Network.AWS.DynamoDB (AttributeValue)
import Qtility

newtype DynamoTableName = DynamoTableName {unDynamoTableName :: Text}
  deriving (Eq, Ord, Show, Read, IsString)

newtype PutItemStatusCode = PutItemStatusCode {unPutItemStatusCode :: Int}
  deriving (Eq, Ord, Show, Read)

newtype GetItemDecodingError = GetItemDecodingError {unGetItemDecodingError :: String}
  deriving (Eq, Ord, Show, Read)

instance Exception GetItemDecodingError

data GetItemParameters = GetItemParameters
  { _getItemParametersTableName :: !DynamoTableName,
    _getItemParametersProjectionExpression :: ![Text],
    _getItemParametersKey :: !(HashMap Text AttributeValue),
    _getItemParametersConsistentRead :: !Bool
  }
  deriving (Eq, Show, Generic)

foldMapM makeLenses [''GetItemParameters]

foldMapM makeWrapped [''DynamoTableName, ''PutItemStatusCode, ''GetItemDecodingError]
