module Domain.Write.JournalEntry where

import "time" Data.Time (Day, UTCTime (..))

import Domain.Write.JournalEntryContent
import Domain.Write.JournalEntryCreatedAt

data JournalEntry = JournalEntry
  { content :: JournalEntryContent
  , createdAt :: JournalEntryCreatedAt
  }
  deriving stock (Eq, Show)

wasCreatedOn :: JournalEntry -> Day -> Bool
wasCreatedOn (JournalEntry _ (JournalEntryCreatedAt (UTCTime createdAtDay _))) day = day == createdAtDay

-- | Check whether the first entry is newer than the second
isNewer :: JournalEntry -> JournalEntry -> Ordering
isNewer (JournalEntry _ createdAt1) (JournalEntry _ createdAt2) = compare createdAt2 createdAt1
