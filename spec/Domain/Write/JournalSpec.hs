{-# LANGUAGE OverloadedStrings #-}

module Domain.Write.JournalSpec where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)
import "time" Data.Time (UTCTime (..), fromGregorian)

import "crem-journal" Domain.Write.Journal (Journal, entryList, logEntry)
import "crem-journal" Domain.Write.JournalEntry (JournalEntry (..))
import "crem-journal" Domain.Write.JournalEntryContent (journalEntryContent)
import "crem-journal" Domain.Write.JournalEntryCreatedAt (JournalEntryCreatedAt (..))

import Test.Helpers (assertRight)

spec :: Spec
spec = describe "Domain.Write.Journal" $ do
  describe "logEntry" $ do
    it "adds entries to the journal keeping them ordered" $ do
      let earlierDate = UTCTime (fromGregorian 2024 12 31) 0
          laterDate = UTCTime (fromGregorian 2025 01 01) 0
      entry1 <- assertRight $ JournalEntry <$> journalEntryContent "content1" <*> pure (JournalEntryCreatedAt earlierDate)
      entry2 <- assertRight $ JournalEntry <$> journalEntryContent "content2" <*> pure (JournalEntryCreatedAt laterDate)
      let journal :: Journal = mempty
          journal' = logEntry entry1 journal
          journal'' = logEntry entry2 journal'
      entryList journal'' `shouldBe` [entry2, entry1] -- first the entry with the later date
