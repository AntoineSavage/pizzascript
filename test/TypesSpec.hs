module TypesSpec where
    
import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Text.Parsec.Pos
import Types

spec :: Spec
spec = do
    metaEqSpec
    metaOrdSpec

metaEqSpec :: Spec
metaEqSpec = describe "Meta (eq)" $ do
    forM_ [p1, p2] $ \p -> do
        it "ignores position" $ do
            property $ \d -> do
                let d1 = d
                    d2 = '_' : d
                (Meta p1 d1) == (Meta p d2) `shouldBe` False
                (Meta p1 d1) == (Meta p d1) `shouldBe` True

metaOrdSpec :: Spec
metaOrdSpec = describe "Meta (ord)" $ do
    forM_ [p1, p2] $ \p -> do
        it "ignores position" $ do
            property $ \d -> do
                let d1 = d
                    d2 = '_' : d
                compare (Meta p1 d1) (Meta p d2) `shouldBe` compare d1 d2
                compare (Meta p1 d1) (Meta p d1) `shouldBe` EQ

p1 = newPos "abc" 1 1
p2 = newPos "xyz" 2 2