{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.SNS.Types where

import Qtility

newtype TopicARN = TopicARN {unTopicARN :: Text}
  deriving (Eq, Show, Read, Generic)

newtype PublishStatusCode = PublishStatusCode {unPublishStatusCode :: Int}
  deriving (Eq, Show, Read, Generic)

foldMapM makeWrapped [''TopicARN, ''PublishStatusCode]
