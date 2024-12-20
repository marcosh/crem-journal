{-# LANGUAGE DataKinds #-}

module Domain.Write.JournalAggregate where

import "crem" Crem.BaseMachine (BaseMachineT, statelessBaseT)
import "crem" Crem.Topology (TrivialTopology)
import "monad-time" Control.Monad.Time (MonadTime)

import Domain.Write.JournalCommand (JournalCommand (..))
import Domain.Write.JournalEntry (JournalEntry (..))
import Domain.Write.JournalEntryCreatedAt (journalCreatedNow)
import Domain.Write.JournalEvent (JournalEvent (..))

aggregate :: (MonadTime m) => BaseMachineT m (TrivialTopology @()) JournalCommand JournalEvent
aggregate = statelessBaseT $ \(RecordJournalEntry content) ->
  JournalEntryRecorded . JournalEntry content <$> journalCreatedNow
