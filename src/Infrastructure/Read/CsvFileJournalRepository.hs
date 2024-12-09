module Infrastructure.Read.CsvFileJournalRepository where

import "base" GHC.Generics (Generic)
import "bytestring" Data.ByteString.Char8 (pack, unpack)
import "cassava" Data.Csv (Field, FromField (..), FromRecord, Parser, ToField (..), ToRecord)
import "text" Data.Text (Text)
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

data CsvRow = JournalCsvRow
  { content :: Text
  , createdAt :: CsvTime
  }
  deriving stock (Generic, Show)

instance FromRecord CsvRow
instance ToRecord CsvRow
