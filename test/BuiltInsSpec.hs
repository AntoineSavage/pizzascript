module BuiltInsSpec where

import qualified Data.Map as M

import Test.Hspec
import Test.QuickCheck

import BuiltIns
import Types
import TestUtils

spec :: Spec
spec = do
    boolishSpec

boolishSpec :: Spec
boolishSpec = describe "boolish" $ do
    it "converts false and true" $ do
        boolish pzFalse `shouldBe` FalseReal
        boolish pzTrue `shouldBe` TrueReal

    it "converts falsish values" $ do
        property $ \p -> do
            boolish (WithPos p PzUnit) `shouldBe` Falsish
            boolish (WithPos p $ PzNum 0) `shouldBe` Falsish
            boolish (WithPos p $ PzStr "") `shouldBe` Falsish
            boolish (WithPos p $ PzList []) `shouldBe` Falsish
            boolish (WithPos p $ PzDict M.empty) `shouldBe` Falsish
    
    it "converts simple truish values" $ do
        property $ \p f -> do
            let unit = WithPos p PzUnit
            boolish (WithPos p $ PzNum 1) `shouldBe` Truish
            boolish (WithPos p $ PzStr "0") `shouldBe` Truish
            boolish (WithPos p $ PzList [unit]) `shouldBe` Truish
            boolish (WithPos p $ PzDict $ M.fromList [(unit, unit)]) `shouldBe` Truish
            boolish (WithPos p $ PzFunc f) `shouldBe` Truish
    
    it "converts truish values" $ do
        property $ \p (NonZero n) (NonEmpty s) (NonEmpty l) d k v f -> do
            let unit = WithPos p PzUnit
            boolish (WithPos p $ PzNum n) `shouldBe` Truish
            boolish (WithPos p $ PzStr s) `shouldBe` Truish
            boolish (WithPos p $ PzList l) `shouldBe` Truish
            boolish (WithPos p $ PzDict $ M.insert k v d) `shouldBe` Truish
            boolish (WithPos p $ PzFunc f) `shouldBe` Truish