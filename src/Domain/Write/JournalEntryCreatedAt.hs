module Domain.Write.JournalEntryCreatedAt where

import "monad-time" Control.Monad.Time (MonadTime (..))
import "time" Data.Time (UTCTime)

newtype JournalEntryCreatedAt = JournalEntryCreatedAt {journalEntryCreatedAtAsUTCTime :: UTCTime}

journalCreatedNow :: (MonadTime m) => m JournalEntryCreatedAt
journalCreatedNow = JournalEntryCreatedAt <$> currentTime
