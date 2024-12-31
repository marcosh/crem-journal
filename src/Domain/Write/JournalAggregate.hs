{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

module Domain.Write.JournalAggregate where

import "crem" Crem.BaseMachine (ActionResult (..), BaseMachineT (..), InitialState (..))
import "crem" Crem.Topology (TrivialTopology)
import "monad-time" Control.Monad.Time (MonadTime)

import Domain.Write.Journal (Journal, logEntry)
import Domain.Write.JournalCommand (JournalCommand (..))
import Domain.Write.JournalEntry (JournalEntry (..))
import Domain.Write.JournalEntryCreatedAt (journalCreatedNow)
import Domain.Write.JournalEvent (JournalEvent (..))

data JournalState (journalVertex :: ()) where
  JournalMapState :: Journal -> JournalState '()

aggregate :: (MonadTime m) => JournalState '() -> BaseMachineT m (TrivialTopology @()) JournalCommand JournalEvent
aggregate initialState =
  BaseMachineT
    { initialState = InitialState initialState
    , action = \(JournalMapState journal) (RecordJournalEntry content) ->
        ActionResult $ do
          journalEntryCreatedAt <- journalCreatedNow
          let entry = JournalEntry content journalEntryCreatedAt
          pure
            ( JournalEntryRecorded entry
            , JournalMapState $ logEntry entry journal
            )
    }
