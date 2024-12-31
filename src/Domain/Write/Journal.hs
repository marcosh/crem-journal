module Domain.Write.Journal
  ( Journal (entryList)
  , logEntry
  , fromEntries
  , applyJournalEvent
  )
where

import Data.List (sortBy)
import Domain.Write.JournalEntry (JournalEntry (..), isNewer)
import Domain.Write.JournalEvent (JournalEvent (..))

-- list of events ordered from the newest to the oldest
newtype Journal = Journal {entryList :: [JournalEntry]}

instance Semigroup Journal where
  (<>) :: Journal -> Journal -> Journal
  (<>) (Journal entries1) (Journal entries2) = Journal . sortBy isNewer $ entries1 <> entries2

instance Monoid Journal where
  mempty :: Journal
  mempty = Journal []

logEntry :: JournalEntry -> Journal -> Journal
logEntry journalEntry (Journal entries) = Journal . sortBy isNewer $ journalEntry : entries

fromEntries :: [JournalEntry] -> Journal
fromEntries = Journal . sortBy isNewer

applyJournalEvent :: Journal -> JournalEvent -> Journal
applyJournalEvent journal (JournalEntryRecorded journalEntry) = logEntry journalEntry journal
