module Infrastructure.Write.EventStorePayload where

import "aeson" Data.Aeson (Value)
import "base" Text.Read (readMaybe)
import "bytestring" Data.ByteString.Char8 (pack, unpack)
import "cassava" Data.Csv (Field, FromField (..), Parser, ToField (..))

newtype EventStorePayload = EventStorePayload Value
  deriving stock (Show)

newtype EventStorePayloadError
  = ImpossibleToParseJSONValue String

instance FromField EventStorePayload where
  parseField :: Field -> Parser EventStorePayload
  parseField field = case readMaybe $ unpack field of
    Nothing -> fail "payload does not contain JSON value"
    Just value -> pure $ EventStorePayload value

instance ToField EventStorePayload where
  toField :: EventStorePayload -> Field
  toField (EventStorePayload value) = pack $ show value
