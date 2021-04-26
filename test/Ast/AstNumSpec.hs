module Ast.AstNumSpec where

import Test.Hspec
import Test.QuickCheck

import Ast.AstNum
import Data.Either
import Text.Parsec

spec :: Spec
spec = do
    parseVsUnparseSpec
    parseSpec
    unparseSpec

parseVsUnparseSpec :: Spec
parseVsUnparseSpec = describe "parse vs unparse" $ do
    it "composes parse and unparse into identity" $ do
        property $ \astNum -> do
            parse parser "tests" (unparse astNum) `shouldBe` Right astNum
            unparse <$> parse parser "tests" (unparse astNum) `shouldBe` Right (unparse astNum)

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "rejects empty string" $ do
        isLeft(parse parser "tests" "") `shouldBe` True

    it "parses integers" $ do
        property $ \n -> do
            parse parser "tests" (show (n :: Int)) `shouldBe` Right (AstNum $ fromIntegral n)

    it "parses doubles with decimal part" $ do
        property $ \intPart (Positive decPart) -> do
            let _ = (intPart :: Integer, decPart :: Integer)
                d = read $ show intPart ++ "." ++ show decPart
            parse parser "tests" (show d) `shouldBe` Right (AstNum d)

    it "parses doubles with exponential part" $ do
        property $ \intPart (Positive decPart) expPart -> do
            let _ = (intPart :: Integer, decPart :: Integer, expPart :: Integer)
                d1 = read (show intPart ++ "." ++ show (decPart + 1) ++ "e" ++ show expPart)
                d2 = read (show intPart ++ "." ++ show (decPart + 1) ++ "E" ++ show expPart)
            parse parser "tests" (show d1) `shouldBe` Right (AstNum d1)
            parse parser "tests" (show d2) `shouldBe` Right (AstNum d2)

    it "parses doubles" $ do
        property $ \d -> do
            parse parser "tests" (show d) `shouldBe` Right (AstNum d)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "returns show<int> for any integer" $ do
        property $ \n -> do
            unparse (AstNum $ fromIntegral n) `shouldBe` show (n :: Int)

    it "returns show<double> for any non-integer" $ do
        property $ \d -> do
            unparse (AstNum $ d + 0.1) `shouldBe` show (d + 0.1)

instance Arbitrary AstNum where
    arbitrary = AstNum <$> arbitrary