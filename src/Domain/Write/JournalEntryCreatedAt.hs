module Domain.Write.JournalEntryCreatedAt where

import "monad-time" Control.Monad.Time (MonadTime (..))
import "time" Data.Time (UTCTime)

newtype JournalEntryCreatedAt = JournalEntryCreatedAt UTCTime

journalCreatedNow :: (MonadTime m) => m JournalEntryCreatedAt
journalCreatedNow = JournalEntryCreatedAt <$> currentTime
