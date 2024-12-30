{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

module Domain.Write.JournalAggregate where

import "crem" Crem.BaseMachine (ActionResult (..), BaseMachineT (..), InitialState (..))
import "crem" Crem.Topology (TrivialTopology)
import "monad-time" Control.Monad.Time (MonadTime)

import Domain.Write.JournalCommand (JournalCommand (..))
import Domain.Write.JournalEntry (JournalEntry (..))
import Domain.Write.JournalEntryCreatedAt (journalCreatedNow)
import Domain.Write.JournalEvent (JournalEvent (..))

data JournalState (journalVertex :: ()) where
  JournalMapState :: [JournalEvent] -> JournalState '()

aggregate :: (MonadTime m) => BaseMachineT m (TrivialTopology @()) JournalCommand JournalEvent
aggregate =
  BaseMachineT
    { initialState = InitialState $ JournalMapState []
    , action = \(JournalMapState events) (RecordJournalEntry content) ->
        ActionResult $ do
          journalEntryCreatedAt <- journalCreatedNow
          let event = JournalEntryRecorded (JournalEntry content journalEntryCreatedAt)
          pure
            ( event
            , JournalMapState $ event : events
            )
    }
