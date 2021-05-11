module Data.WithPosSpec where


import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.WithPos
import Text.Parsec.Pos
import Types

spec :: Spec
spec = do
    withPosShowSpec
    withPosEqSpec
    withPosOrdSpec
    withPosFuncSpec

withPosShowSpec :: Spec
withPosShowSpec = describe "WithPos (Show instance)" $ do
    forM_ positions $ \p -> do
        it "ignores position" $ do
            show (WithPos p1 123) `shouldBe` "123"

        it "ignores position (prop)" $ do
            property $ \x -> do
                let _ = x :: Int
                show (WithPos p1 x) `shouldBe` show x

withPosEqSpec :: Spec
withPosEqSpec = describe "WithPos (Eq instance)" $ do
    forM_ positions $ \p -> do
        it "ignores position, 1 == 1" $ do
            (WithPos p1 1) == (WithPos p 1) `shouldBe` True

        it "ignores position, 1 /= 2" $ do
            (WithPos p1 1) /= (WithPos p 2) `shouldBe` True

        it "ignores position (prop)" $ do
            property $ \x y -> do
                let _ = x :: Int
                (WithPos p1 x) == (WithPos p y) `shouldBe` x == y

withPosOrdSpec :: Spec
withPosOrdSpec = describe "WithPos (Ord instance)" $ do
    forM_ positions $ \p -> do
        it "ignores position, compare 1 2 == LT" $ do
            compare (WithPos p1 1) (WithPos p 2) `shouldBe` LT

        it "ignores position, compare 2 1 == GT" $ do
            compare (WithPos p1 2) (WithPos p 1) `shouldBe` GT

        it "ignores position, compare 1 1 == EQ" $ do
            compare (WithPos p1 1) (WithPos p 1) `shouldBe` EQ

        it "ignores position (prop)" $ do
            property $ \x y -> do
                let _ = x :: Int
                compare (WithPos p1 x) (WithPos p y) `shouldBe` compare x y

withPosFuncSpec :: Spec
withPosFuncSpec = describe "WithPos (Functor instance)" $ do
    forM_ positions $ \p -> do
        it "ignores position" $ do
            fmap (+1) (WithPos p1 1) `shouldBe` (WithPos p 2)

        it "ignores position (prop)" $ do
            property $ \x y -> do
                let _ = (x :: Int, y :: String)
                fmap (const y) (WithPos p1 x) `shouldBe` WithPos p y

positions = [ p1, p2 ]
p1 = newPos "abc" 1 1
p2 = newPos "xyz" 2 2