module Domain.Write.JournalAggregateSpec where

import "base" Control.Monad.IO.Class (liftIO)
import "crem" Crem.BaseMachine (runBaseMachineT)
import "hedgehog" Hedgehog (forAll)
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)
import "hspec-hedgehog" Test.Hspec.Hedgehog (hedgehog)

import "crem-journal" Domain.Write.JournalAggregate (aggregate)
import "crem-journal" Domain.Write.JournalCommand (JournalCommand (..))
import "crem-journal" Domain.Write.JournalEntry (JournalEntry (..))
import "crem-journal" Domain.Write.JournalEvent (JournalEvent (..))

import Gen.JournalEntryContent (genJournalEntryContent)

spec :: Spec
spec = describe "Domain.Write.Aggregate" $ do
  describe "aggregate" $ do
    it "for every RecordJournalEntry command produces a JournalEntryRecorded event with the correct content" $ hedgehog $ do
      commandContent <- forAll genJournalEntryContent
      (JournalEntryRecorded (JournalEntry eventContent _), _) <- liftIO $ runBaseMachineT aggregate (RecordJournalEntry commandContent)
      liftIO $ eventContent `shouldBe` commandContent
