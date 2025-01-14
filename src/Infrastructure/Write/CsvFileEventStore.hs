module Infrastructure.Write.CsvFileEventStore where

import "aeson" Data.Aeson (Result (..), ToJSON (toJSON), fromJSON)
import "base" Control.Arrow (left)
import "base" Control.Monad.IO.Class (MonadIO (..))
import "base" Data.Traversable (for)
import "base" GHC.Generics (Generic)
import "bytestring" Data.ByteString.Lazy qualified as BL
import "cassava" Data.Csv (FromRecord, HasHeader (..), ToRecord, decode, encode)
import "mtl" Control.Monad.Except (MonadError (..), liftEither)
import "mtl" Control.Monad.Reader (MonadReader (..))
import "utf8-string" Data.ByteString.Lazy.UTF8 qualified as BLU
import "vector" Data.Vector (toList)

import Domain.Write.JournalEntry qualified as Entry (JournalEntry (..))
import Domain.Write.JournalEntryCreatedAt (JournalEntryCreatedAt (..))
import Domain.Write.JournalEvent (JournalEvent (..), JournalEventTag (..), journalEventTag)
import Infrastructure.CsvFileError (CsvFileError (..))
import Infrastructure.CsvTime (CsvTime (CsvTime))
import Infrastructure.Write.EventStore (EventStore (..))
import Infrastructure.Write.EventStorePayload (EventStorePayload (..), EventStorePayloadError (..))
import Infrastructure.Write.EventStream (EventStream, fromEventList)

data EventStoreCsvRow = EventStoreCsvRow
  { tag :: JournalEventTag
  , createdAt :: CsvTime
  , payload :: EventStorePayload
  }
  deriving stock (Generic, Show)

instance FromRecord EventStoreCsvRow
instance ToRecord EventStoreCsvRow

newtype CsvFileEventStoreConfig = CsvFileEventStoreConfig
  {csvFilePath :: FilePath}

-- | Our event store based on a csv file
-- It is a wrapper over operations in a context `m`
newtype CsvFileEventStore m a = CsvFileEventStore (m a)

storeEventInCSVFile :: (MonadIO m, MonadReader CsvFileEventStoreConfig m) => JournalEvent -> CsvFileEventStore m ()
storeEventInCSVFile = \case
  JournalEntryRecorded journalEntry -> CsvFileEventStore $ do
    let csvRow =
          EventStoreCsvRow
            (journalEventTag $ JournalEntryRecorded journalEntry)
            (CsvTime . journalEntryCreatedAtAsUTCTime . Entry.createdAt $ journalEntry)
            (EventStorePayload $ toJSON journalEntry)
    config <- ask
    liftIO $ BL.appendFile (csvFilePath config) $ encode [csvRow] -- TODO: handle file related exceptions

retrieveEventsFromCSVFile :: (MonadIO m, MonadReader CsvFileEventStoreConfig m, MonadError (CsvFileError EventStorePayloadError) m) => CsvFileEventStore m (EventStream JournalEvent)
retrieveEventsFromCSVFile = CsvFileEventStore $ do
  config <- ask
  fileContent <- liftIO $ readFile (csvFilePath config) -- TODO: handle file related exceptions
  csvRows <- liftEither . left UnableToParseCsvFile $ decode @EventStoreCsvRow NoHeader $ BLU.fromString fileContent
  fmap fromEventList <$> for (toList csvRows) $ \(EventStoreCsvRow tag _ (EventStorePayload payload)) ->
    case tag of
      JournalEntryRecordedTag -> do
        journalEvent <- case fromJSON payload of
          Error message -> throwError . CsvContentError . ImpossibleToParseJSONValue $ message
          Success journalEvent' -> pure journalEvent'
        pure . JournalEntryRecorded $ journalEvent

instance (MonadIO m, MonadReader CsvFileEventStoreConfig m, MonadError (CsvFileError EventStorePayloadError) m) => EventStore (CsvFileEventStore m) where
  storeEvent = storeEventInCSVFile
  retrieveEvents = retrieveEventsFromCSVFile
