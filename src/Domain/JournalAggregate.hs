{-# LANGUAGE DataKinds #-}

module Domain.JournalAggregate where

import "crem" Crem.BaseMachine (BaseMachineT, statelessBaseT)
import "crem" Crem.Topology (TrivialTopology)

import Domain.JournalCommand (JournalCommand (..))
import Domain.JournalEntryCreatedAt (journalCreatedNow)
import Domain.JournalEvent (JournalEvent (..))

aggregate :: (MonadTime m) => BaseMachineT m (TrivialTopology @()) JournalCommand JournalEvent
aggregate = statelessBaseT $ \(RecordJournalEntry content) -> do
  entryCreatedAt <- journalCreatedNow
  pure $ JournalEntryRecorded content entryCreatedAt
