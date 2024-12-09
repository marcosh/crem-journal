module Gen.UTCTime where

import "hedgehog" Hedgehog (Gen)
import "hedgehog" Hedgehog.Gen (integral)
import "hedgehog" Hedgehog.Range (constant)
import "time" Data.Time (UTCTime (..), fromGregorianValid, secondsToDiffTime)

genUTCTime :: Gen UTCTime
genUTCTime = do
  year <- integral $ constant 0 3000
  month <- integral $ constant 1 12
  day <- integral $ constant 1 31
  case fromGregorianValid year month day of
    Nothing -> genUTCTime -- try again
    Just date -> do
      seconds <- secondsToDiffTime <$> integral (constant 0 86400)
      pure $ UTCTime date seconds
