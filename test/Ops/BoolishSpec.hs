module Ops.BoolishSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.Values
import Control.Monad
import Ops.Boolish
import Ops.PzValSpec
import Symbs
import TestUtils
import Types.Boolish
import Types.PzVal
import Types.Str
import Types.Numb

spec :: Spec
spec = do
    boolishSpec

boolishSpec :: Spec
boolishSpec = describe "boolish" $ do
    it "converts false and true" $ do
        boolish pzSymbFalse `shouldBe` FalseReal
        boolish pzSymbTrue `shouldBe` TrueReal

    it "converts falsish values" $ do
        boolish (PzUnit) `shouldBe` Falsish
        boolish (PzNum $ Numb 0) `shouldBe` Falsish
        boolish (PzStr $ Str "") `shouldBe` Falsish
        boolish (PzList []) `shouldBe` Falsish
        boolish (PzDict M.empty) `shouldBe` Falsish

    it "converts falsish values (prop)" $ do
        property $ \(PzFalsish v) -> do
            boolish v `shouldBe` Falsish
   
    it "converts simple truish values" $ do
        property $ \(ArbDict ctx) f -> do
            let u = PzUnit
            boolish (PzNum $ Numb 1) `shouldBe` Truish
            boolish (PzStr $ Str "0") `shouldBe` Truish
            boolish (PzList [u]) `shouldBe` Truish
            boolish (PzDict $ M.fromList [(u, u)]) `shouldBe` Truish
            boolish (PzFunc ctx f) `shouldBe` Truish
   
    it "converts truish values (prop)" $ do
        property $ \(PzTruish v) -> do
            boolish v `shouldBe` Truish

-- Utils
newtype PzFalsish = PzFalsish PzVal deriving (Show, Eq)
instance Arbitrary PzFalsish where
    arbitrary = do
        fmap PzFalsish $ elements $
            [ PzUnit
            , PzNum $ Numb 0
            , PzStr $ Str ""
            , PzList []
            , PzDict M.empty
            ]

newtype PzTruish = PzTruish PzVal deriving (Show, Eq)
instance Arbitrary PzTruish where
    arbitrary = fmap PzTruish $ oneof
            [ PzNum . Numb . getNonZero <$> arbitrary
            , PzStr . Str . getNonEmpty <$> arbitrary
            , PzList  <$> liftM2 (:) arbDepth (arbFew arbDepth)
            , fmap PzDict $ liftM3 M.insert arbDepth arbDepth arbDepth
            , liftM2 PzFunc arbDepth arbitrary
            ]