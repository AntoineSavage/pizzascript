module Data.NumberSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Number as Number
import Data.Either
import Text.Parsec
import Utils

spec :: Spec
spec = do
  unparseSpec
  parserSpec

unparseSpec = do
  describe "unparse" $ do
    it "equals show for ints" $
      property $ \n -> do
        unparse (NumberInt n) `shouldBe` show n

    it "equals show for floats" $
      property $ \n -> do
        unparse (NumberFloat n) `shouldBe` show n

    it "composes with parser into identity (ints)" $
      property $ \n -> do
        let number = NumberInt n
        parse parser "test" (unparse number) `shouldBe` Right number

    it "composes with parser into identity (floats)" $
      property $ \n -> do
        let number = NumberFloat n
        parse parser "test" (unparse number) `shouldBe` Right number

parserSpec = do
  describe "parser" $ do
    it "parses positive ints" $ do
      forM_ [0..1000] $ \n -> do
        parse parser "test" (show n) `shouldBe` Right (NumberInt n)

    it "parses negative ints" $ do
      forM_ [0..1000] $ \n -> do
        parse parser "test" (show $ -n) `shouldBe` Right (NumberInt $ -n)

    it "parses signed ints" $ do
      property $ \n -> do
        parse parser "test" (show n) `shouldBe` Right (NumberInt n)
      
    it "parses floats" $ do
      forM_ [-2.2, -1.1, -0.0, 0.0, 1.1, 2.2] $ \n -> do
        parse parser "test" (show n) `shouldBe` Right (NumberFloat n)
      
    it "parses floats (prop)"$ do
      property $ \n -> do
        parse parser "test" (show n) `shouldBe` Right (NumberFloat n)

    it "parses floats with scientific notation" $ do
      forM_ [ ("0.0e0", 0), ("1.1E1", 11), ("2.2e2", 220), ("3.3e3", 3300)
            , ("-0.0e0", 0), ("-1.1E1", -11), ("-2.2e2", -220), ("-3.3e3", -3300)
            , ("0.0e-0", 0), ("1.1E-1", 0.11), ("2.2e-2", 0.022), ("3.3e-3", 0.0033)
            , ("-0.0e-0", 0), ("-1.1E-1", -0.11), ("-2.2e-2", -0.022), ("-3.3e-3", -0.0033)
            ] $ \(s,n) -> do
      parse parser "test" s `shouldBe` Right (NumberFloat n)