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
    fromIntOrDoubleSpec

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
            parse parser "tests" (show n) `shouldBe` Right (AstInteger n)

    it "parses doubles with decimal part" $ do
        property $ \intPart (Positive decPart) -> do
            let _ = (intPart :: Integer, decPart :: Integer)
                d = read $ show intPart ++ "." ++ show decPart
            parse parser "tests" (show d) `shouldBe` Right (AstDouble d)

    it "parses doubles with exponential part" $ do
        property $ \intPart (Positive decPart) expPart -> do
            let _ = (intPart :: Integer, decPart :: Integer, expPart :: Integer)
                d1 = read (show intPart ++ "." ++ show (decPart + 1) ++ "e" ++ show expPart)
                d2 = read (show intPart ++ "." ++ show (decPart + 1) ++ "E" ++ show expPart)
            parse parser "tests" (show d1) `shouldBe` Right (fromIntOrDouble AstInteger AstDouble d1)
            parse parser "tests" (show d2) `shouldBe` Right (fromIntOrDouble AstInteger AstDouble d2)

    it "parses doubles" $ do
        property $ \d -> do
            parse parser "tests" (show $ d + 0.1) `shouldBe` Right (AstDouble $ d + 0.1)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "returns show<int> for any int" $ do
        property $ \n -> do
            unparse (AstInteger n) `shouldBe` show n

    it "returns show<int> for any int-valued double" $ do
        property $ \n -> do
            unparse (AstDouble $ fromIntegral n) `shouldBe` show (n :: Integer)

    it "returns show<double> for any non-int-valued double" $ do
        property $ \d -> do
            unparse (AstDouble $ d + 0.1) `shouldBe` show (d + 0.1)

fromIntOrDoubleSpec :: Spec
fromIntOrDoubleSpec = describe "fromIntOrDouble" $ do
    it "applies f to integers" $ do
        property $ \n -> do
            let d = fromIntegral n
            fromIntOrDouble id undefined d `shouldBe` n

    it "applies f to doubles" $ do
        property $ \d_ -> do
            let d = d_ + 0.1
            fromIntOrDouble undefined id d `shouldBe` d

instance Arbitrary AstNum where
    arbitrary = do
        choice <- arbitrary
        if choice
            then AstInteger <$> arbitrary
            else AstDouble . (+0.1) <$> arbitrary