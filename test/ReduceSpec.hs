module ReduceSpec where

import Test.Hspec
import Test.QuickCheck

import Reduce

spec :: Spec
spec = do
    toAccSpec

toAccSpec :: Spec
toAccSpec = describe "toAcc" $ do
    it "handles Evaled" $ do
        pending
    
    it "handles PushForm" $ do
        pending