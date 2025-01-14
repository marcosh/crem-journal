{-# LANGUAGE OverloadedStrings #-}

module Domain.Write.JournalEvent where

import "base" Data.Coerce (coerce)
import "base" Data.Ord (comparing)
import "cassava" Data.Csv (Field, FromField (..), Parser, ToField (..))

import Domain.Write.JournalEntry (JournalEntry (..))

newtype JournalEvent
  = JournalEntryRecorded JournalEntry
  deriving stock (Eq)

-- | This is not a total order
instance Ord JournalEvent where
  compare :: JournalEvent -> JournalEvent -> Ordering
  compare = comparing $ createdAt . coerce

data JournalEventTag
  = JournalEntryRecordedTag
  deriving stock (Show)

instance FromField JournalEventTag where
  parseField :: Field -> Parser JournalEventTag
  parseField "JournalEntryRecordedTag" = pure JournalEntryRecordedTag
  parseField _ = fail "unrecognised JournalEventTag"

instance ToField JournalEventTag where
  toField :: JournalEventTag -> Field
  toField JournalEntryRecordedTag = "JournalEntryRecordedTag"

journalEventTag :: JournalEvent -> JournalEventTag
journalEventTag = \case
  JournalEntryRecorded _ -> JournalEntryRecordedTag
