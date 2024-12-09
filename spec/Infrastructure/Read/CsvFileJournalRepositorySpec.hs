module Infrastructure.Read.CsvFileJournalRepositorySpec where

import "base" Control.Monad.IO.Class (liftIO)
import "cassava" Data.Csv (FromField (..), ToField (..), runParser)
import "hedgehog" Hedgehog (forAll)
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)
import "hspec-hedgehog" Test.Hspec.Hedgehog (hedgehog)

import "crem-journal" Infrastructure.Read.CsvFileJournalRepository (CsvTime (..))

import Gen.UTCTime (genUTCTime)

spec :: Spec
spec = describe "Infrastructure.Read.CsvFileJournalRepository" $ do
  describe "CsvTime FromField and ToField" $
    it "are partially inverse" $
      hedgehog $ do
        csvTime <- CsvTime <$> forAll genUTCTime
        liftIO $ runParser (parseField $ toField csvTime) `shouldBe` Right csvTime
