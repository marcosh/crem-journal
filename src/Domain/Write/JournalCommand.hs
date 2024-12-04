module Domain.Write.JournalCommand where

import Domain.Write.JournalEntryContent (JournalEntryContent)

data JournalCommand
  = RecordJournalEntry JournalEntryContent
