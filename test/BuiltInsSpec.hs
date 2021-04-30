module BuiltInsSpec where

import Test.Hspec

spec :: Spec
spec = do
    describe "moo" $ it "moos" $ 1+1 `shouldBe` 2