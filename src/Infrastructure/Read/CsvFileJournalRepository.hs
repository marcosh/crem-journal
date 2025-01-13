module Infrastructure.Read.CsvFileJournalRepository where

import "base" Control.Arrow (left)
import "base" Control.Monad.IO.Class (MonadIO (..))
import "base" Data.Traversable (for)
import "base" GHC.Generics (Generic)
import "bytestring" Data.ByteString.Lazy qualified as BL
import "cassava" Data.Csv (FromRecord, HasHeader (..), ToRecord, decode, encode)
import "mtl" Control.Monad.Except (MonadError (..), liftEither)
import "mtl" Control.Monad.Reader (MonadReader (..))
import "text" Data.Text (Text)
import "time" Data.Time (Day)
import "utf8-string" Data.ByteString.Lazy.UTF8 qualified as BLU
import "vector" Data.Vector (toList)

import Domain.Read.JournalRepository (JournalRepository (..))
import Domain.Write.JournalEntry (JournalEntry (..), wasCreatedOn)
import Domain.Write.JournalEntryContent (JournalEntryContent (..), JournalEntryContentError (..), journalEntryContent)
import Domain.Write.JournalEntryCreatedAt (JournalEntryCreatedAt (..))
import Infrastructure.CsvTime (CsvTime (..))

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

deriving newtype instance (Functor m) => Functor (CsvFileJournalRepository m)

deriving newtype instance (Applicative m) => Applicative (CsvFileJournalRepository m)

deriving newtype instance (Monad m) => Monad (CsvFileJournalRepository m)

-- | Add a new line to the csv file
addContentToCsvFileJournalRepository :: (MonadIO m, MonadReader CsvFileJournalRepositoryConfig m) => JournalEntry -> CsvFileJournalRepository m ()
addContentToCsvFileJournalRepository (JournalEntry content createdAt) = CsvFileJournalRepository $ do
  let csvRow = JournalCsvRow (journalEntryContentAsText content) (CsvTime $ journalEntryCreatedAtAsUTCTime createdAt)
  config <- ask
  liftIO $ BL.appendFile (csvFilePath config) $ encode [csvRow] -- TODO: handle file related exceptionss

data CsvFileError
  = UnableToParseCsvFile String
  | CsvContentError JournalEntryContentError

-- | Retrieve all content from CSV file
getContentsFromCsvFileJournalRepository :: (MonadIO m, MonadReader CsvFileJournalRepositoryConfig m, MonadError CsvFileError m) => CsvFileJournalRepository m [JournalEntry]
getContentsFromCsvFileJournalRepository = CsvFileJournalRepository $ do
  config <- ask
  fileContent <- liftIO $ readFile (csvFilePath config) -- TODO: handle file related exceptions
  csvRows <- liftEither . left UnableToParseCsvFile $ decode @CsvRow NoHeader $ BLU.fromString fileContent
  for (toList csvRows) $ \(JournalCsvRow content (CsvTime createdAt)) -> do
    journalContent <- liftEither . left CsvContentError $ journalEntryContent content
    pure $ JournalEntry journalContent (JournalEntryCreatedAt createdAt)

-- | Retrieve content from CSV file and then filter only content for the specified day
getContentsForDayFromCsvFileJournalRepository :: (MonadIO m, MonadReader CsvFileJournalRepositoryConfig m, MonadError CsvFileError m) => Day -> CsvFileJournalRepository m [JournalEntry]
getContentsForDayFromCsvFileJournalRepository day =
  filter (`wasCreatedOn` day) <$> getContentsFromCsvFileJournalRepository

instance (MonadIO m, MonadReader CsvFileJournalRepositoryConfig m, MonadError CsvFileError m) => JournalRepository (CsvFileJournalRepository m) where
  addContent = addContentToCsvFileJournalRepository
  getContentsForDay = getContentsForDayFromCsvFileJournalRepository
