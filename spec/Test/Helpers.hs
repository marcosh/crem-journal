module Test.Helpers where

assertRight :: (MonadFail m) => Either a b -> m b
assertRight (Right b) = pure b
assertRight _ = fail "expected Right, got Left"
