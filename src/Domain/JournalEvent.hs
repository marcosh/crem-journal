module Domain.JournalEvent where

import "time" Data.Time (UTCTime)

import Domain.JournalEntryContent (JournalEntryContent)

newtype JournalEntryCreatedAt = JournalEntryCreatedAt UTCTime

data JournalEvent
  = JournalEntryRecorded JournalEntryContent JournalEntryCreatedAt
