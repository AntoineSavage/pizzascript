module Data.PzNumSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Either
import Data.PzNum
import Data.Ratio
import Text.Parsec

spec :: Spec
spec = do
    parseSpec
    unparseSpec

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "parses positive ints" $ do
        parse parser "tests" "0" `shouldBe` Right (PzNum 0 1)
        parse parser "tests" "1" `shouldBe` Right (PzNum 1 1)
        parse parser "tests" "2" `shouldBe` Right (PzNum 2 1)
        parse parser "tests" "3" `shouldBe` Right (PzNum 3 1)
        parse parser "tests" "123" `shouldBe` Right (PzNum 123 1)

    it "parses negative ints" $ do
        parse parser "tests" "-1" `shouldBe` Right (PzNum (-1) 1)
        parse parser "tests" "-2" `shouldBe` Right (PzNum (-2) 1)
        parse parser "tests" "-3" `shouldBe` Right (PzNum (-3) 1)
        parse parser "tests" "-123" `shouldBe` Right (PzNum (-123) 1)

    it "parses any int" $ do
        property $ \n -> do
            parse parser "tests" (show n) `shouldBe` Right (PzNum n 1)

    it "parses fractions" $ do
        parse parser "tests" "0.1" `shouldBe` Right (PzNum 3602879701896397 36028797018963968)
        parse parser "tests" "0.25" `shouldBe` Right (PzNum 1 4)
        parse parser "tests" "0.3333" `shouldBe` Right (PzNum 6004199023210345 18014398509481984)
        parse parser "tests" "0.5" `shouldBe` Right (PzNum 1 2)
        parse parser "tests" "1.0" `shouldBe` Right (PzNum 1 1)
        parse parser "tests" "-0.1" `shouldBe` Right (PzNum (-3602879701896397) 36028797018963968)
        parse parser "tests" "-0.25" `shouldBe` Right (PzNum (-1) 4)
        parse parser "tests" "-0.3333" `shouldBe` Right (PzNum (-6004199023210345) 18014398509481984)
        parse parser "tests" "-0.5" `shouldBe` Right (PzNum (-1) 2)
        parse parser "tests" "-1.0" `shouldBe` Right (PzNum (-1) 1)

    it "parses exponent notation" $ do
        parse parser "tests" "1.2e3" `shouldBe` Right (PzNum 1200 1)
        parse parser "tests" "-1.2E-3" `shouldBe` Right (PzNum (-5534023222112865) 4611686018427387904)

    it "parses any float" $ do
        property $ \f -> do
            isRight (parse parser "tests" $ show (f :: Double)) `shouldBe` True

    it "composes with unparse into id" $ do
        property $ \n -> do
            parse parser "tests" (unparse n) `shouldBe` Right n
            unparse <$> parse parser "tests" (unparse n) `shouldBe` Right (unparse n)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "returns 0 when denom is less than or equal to 0" $ do
        property $ \n (Negative m) -> do
            unparse (PzNum n 0) `shouldBe` "0"
            unparse (PzNum n m) `shouldBe` "0"
    it "returns num when denom is 1" $ do
        property $ \n -> do
            unparse (PzNum n 1) `shouldBe` show n

    it "returns i when i*denom = num " $ do
        property $ \i (Positive denom) -> do
            unparse (PzNum (i*denom) denom) `shouldBe` show i

    it "returns floating-point when i*denom /= num " $ do
        property $ \i (Positive denomPred) -> do
            let denom = denomPred + 1
                num = i * denom + 1
                f = fromIntegral num / fromIntegral denom
            unparse (PzNum num denom) `shouldBe` show f

instance Arbitrary PzNum where
    arbitrary = do
        n <- arbitrary
        (Positive d) <- arbitrary
        let nf = fromIntegral (n :: Integer)
            df = fromIntegral (d :: Integer)
            f = nf / df
            r  = realToFrac f
        return $ PzNum (numerator r) (denominator r)