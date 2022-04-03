{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.DynamoDB.Types where

import Network.AWS.DynamoDB (AttributeValue)
import Qtility

newtype DynamoTableName = DynamoTableName {unDynamoTableName :: Text}
  deriving (Eq, Ord, Show, Read, IsString)

newtype DynamoKey = DynamoKey {unDynamoKey :: HashMap Text AttributeValue}
  deriving (Eq, Show, Read)

newtype PutItemStatusCode = PutItemStatusCode {unPutItemStatusCode :: Int}
  deriving (Eq, Ord, Show, Read)

newtype GetItemDecodingError = GetItemDecodingError {unGetItemDecodingError :: String}
  deriving (Eq, Ord, Show, Read)

instance Exception GetItemDecodingError

data GetItemOptions = GetItemOptions
  { _getItemOptionsProjectionExpression :: ![Text],
    _getItemOptionsConsistentRead :: !Bool
  }
  deriving (Eq, Show, Generic)

foldMapM makeLenses [''GetItemOptions]

foldMapM makeWrapped [''DynamoTableName, ''PutItemStatusCode, ''GetItemDecodingError, ''DynamoKey]
