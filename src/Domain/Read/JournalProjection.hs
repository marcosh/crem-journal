{-# LANGUAGE DataKinds #-}

module Domain.Read.JournalProjection where

import "crem" Crem.BaseMachine (BaseMachineT, statelessBaseT)
import "crem" Crem.Topology (TrivialTopology)

import Domain.Read.JournalRepository (JournalRepository (..))
import Domain.Write.JournalEvent (JournalEvent (..))

-- this machine is not producing any output, information will be retrieved via queries
projection :: (JournalRepository m) => BaseMachineT m (TrivialTopology @()) JournalEvent ()
projection = statelessBaseT $ \case
  JournalEntryRecorded content entryCreatedAt -> do
    addContent content entryCreatedAt
