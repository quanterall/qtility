module Network.AWS.QAWS.DynamoDB.Class
  ( ToAttributeValue (..),
    ToAttributeValueMap (..),
    FromAttributeValueMap (..),
    FromAttributeValue (..),
  )
where

import Data.Aeson.Types
import Data.Scientific (floatingOrInteger)
import Data.Typeable (typeRep)
import qualified Network.AWS.DynamoDB as DynamoDB
import Qtility
import Qtility.Time.Types (Milliseconds (..))
import qualified RIO.HashMap as HashMap
import RIO.Partial (fromJust)
import qualified RIO.Vector as Vector

class ToAttributeValueMap a where
  toAttributeValueMap :: a -> HashMap Text DynamoDB.AttributeValue

class FromAttributeValueMap a where
  fromAttributeValueMap :: HashMap Text DynamoDB.AttributeValue -> Either String a

class ToAttributeValue a where
  toAttributeValue :: a -> DynamoDB.AttributeValue
  default toAttributeValue :: (ToJSON a) => a -> DynamoDB.AttributeValue
  toAttributeValue = toJSON >>> toAttributeValue

class FromAttributeValue a where
  fromAttributeValue :: DynamoDB.AttributeValue -> Either String a
  default fromAttributeValue ::
    (FromJSON a, Typeable a) =>
    DynamoDB.AttributeValue ->
    Either String a
  fromAttributeValue =
    fromAttributeValue @Value
      >=> ( fromJSON >>> \case
              Success a -> Right a
              Error e ->
                Left $ "Error parsing JSON for type '" <> show (typeRep (Proxy @a)) <> "': " <> e
          )

instance FromAttributeValue Value where
  fromAttributeValue v
    | v ^. DynamoDB.avNULL == Just True = Right Null
    | isJust (v ^. DynamoDB.avBOOL) = Right (Bool (v ^. DynamoDB.avBOOL & fromJust))
    | isJust (v ^. DynamoDB.avN) = do
      n <- v ^? DynamoDB.avN . _Just & note "Number value not present in value despite guard"
      decodedNumber <- n & encodeUtf8 & eitherDecodeStrict
      case decodedNumber of
        Left e -> e & realToFrac @Double & Number & Right
        Right e -> e & fromInteger & Number & Right
    | isJust (v ^. DynamoDB.avS) = Right $ String (v ^. DynamoDB.avS & fromJust)
    | not (null $ v ^. DynamoDB.avSS) =
      v ^. DynamoDB.avSS & map String & Vector.fromList & Array & Right
    | not (null $ v ^. DynamoDB.avNS) = do
      xs <- v ^. DynamoDB.avNS & mapM (encodeUtf8 >>> eitherDecodeStrict >>> fmap Number)
      xs & Vector.fromList & Array & Right
    | isJust (v ^. DynamoDB.avB) = Right $ String (v ^. DynamoDB.avB & fromJust & decodeUtf8Lenient)
    | not (null $ v ^. DynamoDB.avBS) =
      v ^. DynamoDB.avBS & map (decodeUtf8Lenient >>> String) & Vector.fromList & Array & Right
    | not (HashMap.null $ v ^. DynamoDB.avM) = do
      newItems <-
        v ^. DynamoDB.avM
          & HashMap.toList
          & mapM
            ( \(k, v') -> do
                newItem <- fromAttributeValue v'
                pure (k, newItem)
            )
      newItems & HashMap.fromList & Object & Right
    | isJust (v ^. DynamoDB.avNULL) = Right Null
    | otherwise = Left $ "No matching attribute value type found: " <> show v

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
