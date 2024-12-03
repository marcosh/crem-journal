module Domain.JournalEvent where

import Domain.JournalEntryContent (JournalEntryContent)
import Domain.JournalEntryCreatedAt (JournalEntryCreatedAt)

data JournalEvent
  = JournalEntryRecorded JournalEntryContent JournalEntryCreatedAt
