module Domain.Write.JournalEntry where

import "time" Data.Time (Day, UTCTime (..))

import Domain.Write.JournalEntryContent
import Domain.Write.JournalEntryCreatedAt

data JournalEntry = JournalEntry
  { content :: JournalEntryContent
  , createdAt :: JournalEntryCreatedAt
  }

wasCreatedOn :: JournalEntry -> Day -> Bool
wasCreatedOn (JournalEntry _ (JournalEntryCreatedAt (UTCTime createdAtDay _))) day = day == createdAtDay

isNewer :: JournalEntry -> JournalEntry -> Ordering
isNewer (JournalEntry _ createdAt1) (JournalEntry _ createdAt2) = compare createdAt1 createdAt2
