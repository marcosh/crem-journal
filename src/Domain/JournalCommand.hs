module Domain.JournalCommand where

import Domain.JournalEntryContent (JournalEntryContent)

data JournalCommand
  = RecordJournalEntry JournalEntryContent
