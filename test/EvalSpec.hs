module EvalSpec where

import Test.Hspec
import Test.QuickCheck

-- TODO
spec :: Spec
spec = describe "moo" $ it "moo" $ 1+1 `shouldBe` 2