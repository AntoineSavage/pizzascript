module Ast.AstListSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Ast.AstList
import Text.Parsec

spec :: Spec
spec = do
    parseVsUnparseSpec
    parseSpec
    unparseSpec

parseVsUnparseSpec :: Spec
parseVsUnparseSpec = describe "parse vs unparse" $ do
    it "composes parse and unparse into id" $ do
        property $ \pzList' -> do
            let AstList xs = pzList'
                pzList = AstList $ map getPositive xs
            parse (parser w p) "tests" (unparse " " show $ pzList) `shouldBe` Right pzList
            unparse " " show <$> parse (parser w p) "tests" (unparse " " show $ pzList)
                `shouldBe` Right (unparse " " show pzList)
            
parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "rejects an empty string" $ do
        isLeft (parse (parser w p) "tests" "") `shouldBe` True

    it "parses empty list" $ do
        parse (parser w p) "tests" "[]" `shouldBe` Right (AstList [])
        parse (parser w p) "tests" "[ ]" `shouldBe` Right (AstList [])

    it "parses one element" $ do
        parse (parser w p) "tests" "[1]" `shouldBe` Right (AstList [1])
        parse (parser w p) "tests" "[ 1]" `shouldBe` Right (AstList [1])
        parse (parser w p) "tests" "[1 ]" `shouldBe` Right (AstList [1])
        parse (parser w p) "tests" "[ 1 ]" `shouldBe` Right (AstList [1])

    it "parses two elements" $ do
        parse (parser w p) "tests" "[1 2]" `shouldBe` Right (AstList [1, 2])
        parse (parser w p) "tests" "[ 1 2]" `shouldBe` Right (AstList [1, 2])
        parse (parser w p) "tests" "[1 2 ]" `shouldBe` Right (AstList [1, 2])
        parse (parser w p) "tests" "[ 1 2 ]" `shouldBe` Right (AstList [1, 2])

    it "parses n elements" $ do
        property $ \xs' -> do
            let xs = map getPositive xs'
                s = replace ',' ' ' $ show xs
            parse (parser w p) "tests" s `shouldBe` Right (AstList xs)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses empty brackets for empty elements" $ do
        unparse " " id (AstList []) `shouldBe` "[]"

    it "unparses with one element" $ do
        unparse " " show (AstList [1]) `shouldBe` "[1]"

    it "unparses with two elements" $ do
        unparse " " show (AstList [1, 2]) `shouldBe` "[1 2]"

    it "unparses with n elements" $ do
        property $ \xs -> do
            unparse " " show (AstList (xs :: [Int])) `shouldBe` replace ',' ' ' (show xs)

w = spaces
p = (read :: String -> Int) <$> many1 digit 

replace _   _   []     = []
replace old new (x:xs) =
    let x' = if x == old then new else x
    in x' : replace old new xs

instance Arbitrary a => Arbitrary (AstList a) where
    arbitrary = AstList <$> listOf arbitrary