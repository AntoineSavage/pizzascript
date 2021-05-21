module Data.BoolishSpec where

import qualified Data.Map as M

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Boolish
import Data.Numb
import Data.PzVal
import Data.PzValSpec
import Data.Str
import Data.WithPos
import TestUtils
import Values

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
            boolish (WithPos p $ PzNum $ Numb 0) `shouldBe` Falsish
            boolish (WithPos p $ PzStr $ Str "") `shouldBe` Falsish
            boolish (WithPos p $ PzList []) `shouldBe` Falsish
            boolish (WithPos p $ PzDict M.empty) `shouldBe` Falsish

    it "converts falsish values (prop)" $ do
        property $ \(PzFalsish v) -> do
            boolish v `shouldBe` Falsish
   
    it "converts simple truish values" $ do
        property $ \(ArbDict ctx) p f -> do
            let unit = WithPos p PzUnit
            boolish (WithPos p $ PzNum $ Numb 1) `shouldBe` Truish
            boolish (WithPos p $ PzStr $ Str "0") `shouldBe` Truish
            boolish (WithPos p $ PzList [unit]) `shouldBe` Truish
            boolish (WithPos p $ PzDict $ M.fromList [(unit, unit)]) `shouldBe` Truish
            boolish (WithPos p $ PzFunc ctx f) `shouldBe` Truish
   
    it "converts truish values (prop)" $ do
        property $ \(PzTruish v) -> do
            boolish v `shouldBe` Truish

-- Utils
newtype PzFalsish = PzFalsish (WithPos PzVal) deriving (Show, Eq)
instance Arbitrary PzFalsish where
    arbitrary = do
        p <- arbitrary
        fmap PzFalsish $ elements $ map (WithPos p) $
            [ PzUnit
            , PzNum $ Numb 0
            , PzStr $ Str ""
            , PzList []
            , PzDict M.empty
            ]

newtype PzTruish = PzTruish (WithPos PzVal) deriving (Show, Eq)
instance Arbitrary PzTruish where
    arbitrary = fmap PzTruish $ liftM2 WithPos arbitrary $ oneof
            [ PzNum . Numb . getNonZero <$> arbitrary
            , PzStr . Str . getNonEmpty <$> arbitrary
            , PzList  <$> liftM2 (:) arbDepth (arbFew arbDepth)
            , fmap PzDict $ liftM3 M.insert arbDepth arbDepth arbDepth
            , liftM2 PzFunc arbDepth arbitrary
            ]