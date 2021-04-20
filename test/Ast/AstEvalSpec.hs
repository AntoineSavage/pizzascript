module Ast.AstEvalSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Ast.AstEval
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
            let AstEval xs = pzList'
                pzList = AstEval $ map getPositive xs
            parse (parser w p) "tests" (unparse " " show $ pzList) `shouldBe` Right pzList
            unparse " " show <$> parse (parser w p) "tests" (unparse " " show $ pzList)
                `shouldBe` Right (unparse " " show pzList)
            
parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "rejects an empty string" $ do
        isLeft (parse (parser w p) "tests" "") `shouldBe` True

    it "parses empty eval" $ do
        parse (parser w p) "tests" "()" `shouldBe` Right (AstEval [])
        parse (parser w p) "tests" "( )" `shouldBe` Right (AstEval [])

    it "parses one element" $ do
        parse (parser w p) "tests" "(1)" `shouldBe` Right (AstEval [1])
        parse (parser w p) "tests" "( 1)" `shouldBe` Right (AstEval [1])
        parse (parser w p) "tests" "(1 )" `shouldBe` Right (AstEval [1])
        parse (parser w p) "tests" "( 1 )" `shouldBe` Right (AstEval [1])

    it "parses two elements" $ do
        parse (parser w p) "tests" "(1 2)" `shouldBe` Right (AstEval [1, 2])
        parse (parser w p) "tests" "( 1 2)" `shouldBe` Right (AstEval [1, 2])
        parse (parser w p) "tests" "(1 2 )" `shouldBe` Right (AstEval [1, 2])
        parse (parser w p) "tests" "( 1 2 )" `shouldBe` Right (AstEval [1, 2])

    it "parses n elements" $ do
        property $ \xs' -> do
            let xs = map getPositive xs'
                s = listToEval $ show xs
            parse (parser w p) "tests" s `shouldBe` Right (AstEval xs)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses empty brackets for empty elements" $ do
        unparse " " id (AstEval []) `shouldBe` "()"

    it "unparses with one element" $ do
        unparse " " show (AstEval [1]) `shouldBe` "(1)"

    it "unparses with two elements" $ do
        unparse " " show (AstEval [1, 2]) `shouldBe` "(1 2)"

    it "unparses with n elements" $ do
        property $ \xs -> do
            unparse " " show (AstEval (xs :: [Int])) `shouldBe` listToEval (show xs)

w = spaces
p = (read :: String -> Int) <$> many1 digit 

listToEval = replace '[' '(' . replace ']' ')' . replace ',' ' '

replace _   _   []     = []
replace old new (x:xs) =
    let x' = if x == old then new else x
    in x' : replace old new xs

instance Arbitrary a => Arbitrary (AstEval a) where
    arbitrary = AstEval <$> listOf arbitrary