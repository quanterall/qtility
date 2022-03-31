{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.DynamoDB.Types where

import Qtility

newtype DynamoTableName = DynamoTableName {unDynamoTableName :: Text}
  deriving (Eq, Ord, Show, Read, IsString)

newtype PutItemStatusCode = PutItemStatusCode {unPutItemStatusCode :: Int}
  deriving (Eq, Ord, Show, Read)

foldMapM makeWrapped [''DynamoTableName, ''PutItemStatusCode]
