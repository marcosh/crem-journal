{-# LANGUAGE TemplateHaskell #-}

module Domain.Write.JournalEntry where

import "aeson" Data.Aeson.TH (defaultOptions, deriveJSON)
import "time" Data.Time (Day, UTCTime (..))

import Domain.Write.JournalEntryContent
import Domain.Write.JournalEntryCreatedAt

data JournalEntry = JournalEntry
  { content :: JournalEntryContent
  , createdAt :: JournalEntryCreatedAt
  }
  deriving stock (Eq, Show)

$(deriveJSON defaultOptions ''JournalEntry)

wasCreatedOn :: JournalEntry -> Day -> Bool
wasCreatedOn (JournalEntry _ (JournalEntryCreatedAt (UTCTime createdAtDay _))) day = day == createdAtDay

-- | Check whether the first entry is newer than the second
isNewer :: JournalEntry -> JournalEntry -> Ordering
isNewer (JournalEntry _ createdAt1) (JournalEntry _ createdAt2) = compare createdAt2 createdAt1
