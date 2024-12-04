module Domain.Read.JournalRepository where

import Domain.Write.JournalEntryContent (JournalEntryContent)
import Domain.Write.JournalEntryCreatedAt (JournalEntryCreatedAt)

class (Applicative m) => JournalRepository m where
  addContent :: JournalEntryContent -> JournalEntryCreatedAt -> m ()
