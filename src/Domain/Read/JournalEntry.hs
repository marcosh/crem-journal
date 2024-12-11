module Domain.Read.JournalEntry where

import "time" Data.Time (Day, UTCTime (..))

import Domain.Write.JournalEntryContent
import Domain.Write.JournalEntryCreatedAt

data JournalEntry = JournalEntry
  { content :: JournalEntryContent
  , createdAt :: JournalEntryCreatedAt
  }

wasCreatedOn :: JournalEntry -> Day -> Bool
wasCreatedOn (JournalEntry _ (JournalEntryCreatedAt (UTCTime createdAtDay _))) day = day == createdAtDay
