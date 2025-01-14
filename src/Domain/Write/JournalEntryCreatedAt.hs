{-# LANGUAGE TemplateHaskell #-}

module Domain.Write.JournalEntryCreatedAt where

import "aeson" Data.Aeson.TH (defaultOptions, deriveJSON)
import "monad-time" Control.Monad.Time (MonadTime (..))
import "time" Data.Time (UTCTime)

newtype JournalEntryCreatedAt = JournalEntryCreatedAt {journalEntryCreatedAtAsUTCTime :: UTCTime}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

$(deriveJSON defaultOptions ''JournalEntryCreatedAt)

journalCreatedNow :: (MonadTime m) => m JournalEntryCreatedAt
journalCreatedNow = JournalEntryCreatedAt <$> currentTime
