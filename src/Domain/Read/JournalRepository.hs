module Domain.Read.JournalRepository where

import "time" Data.Time (Day)

import Domain.Read.JournalEntry

class (Applicative m) => JournalRepository m where
  addContent :: JournalEntry -> m ()
  getContentsForDay :: Day -> m [JournalEntry]
