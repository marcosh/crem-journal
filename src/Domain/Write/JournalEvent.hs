module Domain.Write.JournalEvent where

import Domain.Write.JournalEntryContent (JournalEntryContent)
import Domain.Write.JournalEntryCreatedAt (JournalEntryCreatedAt)

data JournalEvent
  = JournalEntryRecorded JournalEntryContent JournalEntryCreatedAt
