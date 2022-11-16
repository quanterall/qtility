module Qtility.Testing.Web where

import Network.HTTP.Types.Method
import Network.Wai.Test (SResponse (..))
import Qtility
import Test.Hspec.Wai

postTo :: (ToJSON a) => ByteString -> a -> WaiSession st SResponse
postTo path value =
  request methodPost path [("Content-Type", "application/json")] (encode value)

putTo :: (ToJSON a) => ByteString -> a -> WaiSession st SResponse
putTo path value =
  request methodPut path [("Content-Type", "application/json")] (encode value)

decodedResponse :: (FromJSON a) => SResponse -> a
decodedResponse response = case eitherDecode $ simpleBody response of
  Left err -> error err
  Right val -> val

equalDecoded :: (FromJSON a, Eq a, Show a) => ResponseMatcher -> a -> ResponseMatcher
equalDecoded matcher a = matcher {matchBody = MatchBody $ matchDecodedValue a}
  where
    matchDecodedValue a' _headers body =
      case eitherDecode body of
        Left e -> Just $ "Decoding error: " <> e
        Right value
          | value == a' -> Nothing
          | otherwise -> Just $ "Expected: " ++ show a' ++ "\nActual: " ++ show value
