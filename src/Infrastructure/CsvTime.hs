module Infrastructure.CsvTime where

import "bytestring" Data.ByteString.Char8 (pack, unpack)
import "cassava" Data.Csv (Field, FromField (..), Parser, ToField (..))
import "time" Data.Time (UTCTime)
import "time" Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)

newtype CsvTime = CsvTime UTCTime
  deriving stock (Show)
  deriving newtype (Eq)

instance FromField CsvTime where
  parseField :: Field -> Parser CsvTime
  parseField field =
    case iso8601ParseM $ unpack field of
      Nothing -> fail "date not in ISO8601 format"
      Just time -> pure $ CsvTime time

instance ToField CsvTime where
  toField :: CsvTime -> Field
  toField (CsvTime time) = pack $ iso8601Show time
