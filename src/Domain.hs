module Domain where

import "text" Data.Text (Text)
import "time" Data.Time (UTCTime)

newtype JournalEntryContent = JournalEntryContent Text

newtype JournalEntryCreatedAt = JournalEntryCreatedAt UTCTime

data JournalCommand
  = RecordJournalEntry JournalEntryContent

data JournalEvent
  = JournalEntryRecorded JournalEntryContent JournalEntryCreatedAt
