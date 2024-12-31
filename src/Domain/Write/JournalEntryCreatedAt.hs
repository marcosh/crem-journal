module Domain.Write.JournalEntryCreatedAt where

import "monad-time" Control.Monad.Time (MonadTime (..))
import "time" Data.Time (UTCTime)

newtype JournalEntryCreatedAt = JournalEntryCreatedAt {journalEntryCreatedAtAsUTCTime :: UTCTime}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

journalCreatedNow :: (MonadTime m) => m JournalEntryCreatedAt
journalCreatedNow = JournalEntryCreatedAt <$> currentTime
