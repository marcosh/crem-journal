{-# LANGUAGE OverloadedStrings #-}

module Domain.Write.JournalEntrySpec where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)
import "time" Data.Time (UTCTime (..), fromGregorian)

import "crem-journal" Domain.Write.JournalEntry (JournalEntry (..), isNewer)
import "crem-journal" Domain.Write.JournalEntryContent (journalEntryContent)
import "crem-journal" Domain.Write.JournalEntryCreatedAt (JournalEntryCreatedAt (..))

spec :: Spec
spec = describe "Domain.Write.JournalEntry" $ do
  describe "isNewer" $ do
    it "correctly selects the newer entry" $ do
      let earlierDate = UTCTime (fromGregorian 2024 12 31) 0
          laterDate = UTCTime (fromGregorian 2025 01 01) 0
          entry1 = JournalEntry <$> journalEntryContent "content1" <*> pure (JournalEntryCreatedAt earlierDate)
          entry2 = JournalEntry <$> journalEntryContent "content2" <*> pure (JournalEntryCreatedAt laterDate)
      isNewer <$> entry1 <*> entry2 `shouldBe` Right GT
