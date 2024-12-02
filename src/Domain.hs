module Domain where

import "time" Data.Time (UTCTime)

import Domain.JournalEntryContent (JournalEntryContent)

newtype JournalEntryCreatedAt = JournalEntryCreatedAt UTCTime

data JournalCommand
  = RecordJournalEntry JournalEntryContent

data JournalEvent
  = JournalEntryRecorded JournalEntryContent JournalEntryCreatedAt
