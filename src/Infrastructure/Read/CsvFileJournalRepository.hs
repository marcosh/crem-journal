module Infrastructure.Read.CsvFileJournalRepository where

import "base" Control.Monad.IO.Class (MonadIO (..))
import "base" GHC.Generics (Generic)
import "bytestring" Data.ByteString.Char8 (pack, unpack)
import "bytestring" Data.ByteString.Lazy qualified as BL
import "cassava" Data.Csv (Field, FromField (..), FromRecord, Parser, ToField (..), ToRecord, encode)
import "mtl" Control.Monad.Reader (MonadReader (..))
import "text" Data.Text (Text)
import "time" Data.Time (UTCTime)
import "time" Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)

import Domain.Write.JournalEntryContent (JournalEntryContent (journalEntryContentAsText))
import Domain.Write.JournalEntryCreatedAt (JournalEntryCreatedAt (..))

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

newtype CsvFileJournalRepositoryConfig = CsvFileJournalRepositoryConfig
  {csvFilePath :: FilePath}

-- | Our journal repository based on a csv file
-- It is a wrapper over operations in a context `m`
newtype CsvFileJournalRepository m a = CsvFileJournalRepository (m a)

addContentToCsvFileJournalRepository :: (MonadIO m, MonadReader CsvFileJournalRepositoryConfig m) => JournalEntryContent -> JournalEntryCreatedAt -> CsvFileJournalRepository m ()
addContentToCsvFileJournalRepository content createdAt = CsvFileJournalRepository $ do
  let csvRow = JournalCsvRow (journalEntryContentAsText content) (CsvTime $ journalEntryCreatedAtAsUTCTime createdAt)
  config <- ask
  liftIO $ BL.appendFile (csvFilePath config) $ encode [csvRow]
