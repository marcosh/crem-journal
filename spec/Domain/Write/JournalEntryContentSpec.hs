module Domain.Write.JournalEntryContentSpec where

import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Either (isRight)
import "hedgehog" Hedgehog (Gen, forAll)
import "hedgehog" Hedgehog.Gen (alphaNum, element, text)
import "hedgehog" Hedgehog.Range (constant)
import "hspec" Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import "hspec-hedgehog" Test.Hspec.Hedgehog (hedgehog)
import "text" Data.Text (Text)

import Domain.Write.JournalEntryContent (JournalEntryContentError (..), journalEntryContent)

genStringWithNoContent :: Gen Text
genStringWithNoContent = text (constant 0 10) $ element "\n\r\t\f\v "

genStringWithContent :: Gen Text
genStringWithContent = text (constant 1 10) $ alphaNum

spec :: Spec
spec = describe "Domain.Write.JournalEntryContent" $ do
  describe "journalEntryContent" $ do
    it "fails to create a JournalEntryContent for a string made just with empty characters" $ hedgehog $ do
      stringWithNoContent <- forAll genStringWithNoContent
      liftIO $ journalEntryContent stringWithNoContent `shouldBe` Left EmptyAfterTrimming

    it "succeeds at creating a JournalEntryContent for a string which does not contain only empty characters" $ hedgehog $ do
      stringWithContent <- forAll genStringWithContent
      liftIO $ journalEntryContent stringWithContent `shouldSatisfy` isRight
