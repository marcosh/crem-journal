module Domain.Write.JournalEvent where

import Domain.Write.JournalEntry (JournalEntry)

newtype JournalEvent
  = JournalEntryRecorded JournalEntry
