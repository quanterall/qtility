module Network.AWS.QAWS.DynamoDB.Class
  ( ToAttributeValue (..),
    ToAttributeValueMap (..),
  )
where

import Data.Scientific (floatingOrInteger)
import qualified Network.AWS.DynamoDB as DynamoDB
import Qtility
import Qtility.Time.Types (Milliseconds (..))

class ToAttributeValueMap a where
  toAttributeValueMap :: a -> HashMap Text DynamoDB.AttributeValue

class ToAttributeValue a where
  toAttributeValue :: a -> DynamoDB.AttributeValue
  default toAttributeValue :: (ToJSON a) => a -> DynamoDB.AttributeValue
  toAttributeValue = toJSON >>> toAttributeValue

instance ToAttributeValue Int where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avN ?~ tshow v

instance ToAttributeValue Integer where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avN ?~ tshow v

instance ToAttributeValue Float

instance ToAttributeValue Double where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avN ?~ tshow v

instance ToAttributeValue Bool where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avBOOL ?~ v

instance ToAttributeValue String where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avS ?~ fromString v

instance ToAttributeValue Text where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avS ?~ v

instance ToAttributeValue ByteString where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avB ?~ v

instance ToAttributeValue LByteString where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avB ?~ toStrictBytes v

instance ToAttributeValue [Int] where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avNS .~ map tshow v

instance ToAttributeValue [Integer] where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avNS .~ map tshow v

instance ToAttributeValue [Float] where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avNS .~ map tshow v

instance ToAttributeValue [Double] where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avNS .~ map tshow v

instance ToAttributeValue [String] where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avSS .~ map fromString v

instance ToAttributeValue [Text] where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avSS .~ v

instance ToAttributeValue [ByteString] where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avBS .~ v

instance ToAttributeValue [LByteString] where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avBS .~ map toStrictBytes v

instance (ToAttributeValue v) => ToAttributeValue (HashMap Text v) where
  toAttributeValue v = DynamoDB.attributeValue & DynamoDB.avM .~ fmap toAttributeValue v

instance (ToAttributeValue a) => ToAttributeValue (Milliseconds a) where
  toAttributeValue (Milliseconds a) = toAttributeValue a

instance (ToAttributeValue a) => ToAttributeValue (Maybe a) where
  toAttributeValue Nothing = DynamoDB.attributeValue & DynamoDB.avNULL ?~ True
  toAttributeValue (Just a) = toAttributeValue a

instance (ToAttributeValue a) => ToAttributeValue (Vector a) where
  toAttributeValue v =
    let list = v & fmap toAttributeValue & toList
     in DynamoDB.attributeValue & DynamoDB.avL .~ list

instance ToAttributeValue Value where
  toAttributeValue (Object m) = toAttributeValue m
  toAttributeValue (String s) = toAttributeValue s
  toAttributeValue (Array a) = toAttributeValue a
  toAttributeValue (Number n) = case floatingOrInteger n of
    Left d -> toAttributeValue @Double d
    Right i -> toAttributeValue @Integer i
  toAttributeValue (Bool b) = toAttributeValue b
  toAttributeValue Null = DynamoDB.attributeValue & DynamoDB.avNULL ?~ True
