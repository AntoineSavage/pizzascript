module Data.NumbSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Either
import Data.Numb
import Text.Parsec

spec :: Spec
spec = do
    parseNumbVsUnparseNumbSpec
    parseNumbSpec
    unparseNumbSpec

parseNumbVsUnparseNumbSpec :: Spec
parseNumbVsUnparseNumbSpec = describe "parseNumb vs unparseNumb" $ do
    it "composes parseNumb and unparseNumb into identity" $ do
        property $ \n -> do
            let s = unparseNumb n
            parse parseNumb "tests" s `shouldBe` Right n
            unparseNumb <$> parse parseNumb "tests" s `shouldBe` Right s

parseNumbSpec :: Spec
parseNumbSpec = describe "parseNumb" $ do
    it "rejects empty string" $ do
        isLeft (parse parseNumb "tests" "") `shouldBe` True

    it "parses integers" $ do
        property $ \n -> do
            parse parseNumb "tests" (show (n :: Int)) `shouldBe` Right (Numb $ fromIntegral n)

    it "parses doubles with decimal part" $ do
        property $ \intPart (Positive decPart) -> do
            let _ = (intPart :: Integer, decPart :: Integer)
                d = read $ show intPart ++ "." ++ show decPart
            parse parseNumb "tests" (show d) `shouldBe` Right (Numb d)

    it "parses doubles with exponential part" $ do
        property $ \intPart (Positive decPart) expPart -> do
            let _ = (intPart :: Integer, decPart :: Integer, expPart :: Integer)
                s1 = show intPart ++ "." ++ show (decPart + 1) ++ "e" ++ show expPart
                s2 = show intPart ++ "." ++ show (decPart + 1) ++ "E" ++ show expPart
                d1 = read s1
                d2 = read s2
            parse parseNumb "tests" s1 `shouldBe` Right (Numb d1)
            parse parseNumb "tests" s1 `shouldBe` Right (Numb d2)

    it "parses doubles" $ do
        property $ \d -> do
            parse parseNumb "tests" (show d) `shouldBe` Right (Numb d)

unparseNumbSpec :: Spec
unparseNumbSpec = describe "unparseNumb" $ do
    it "returns show<int> for any integer" $ do
        property $ \n -> do
            unparseNumb (Numb $ fromIntegral n) `shouldBe` show (n :: Int)

    it "returns show<double> for any non-integer" $ do
        property $ \d -> do
            unparseNumb (Numb $ d + 0.1) `shouldBe` show (d + 0.1)

-- Utils
instance Arbitrary Numb where arbitrary = Numb <$> arbitrary