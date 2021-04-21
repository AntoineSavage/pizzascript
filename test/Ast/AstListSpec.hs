module Ast.AstListSpec where

import Test.Hspec
import Test.QuickCheck

import Ast.AstList
import Control.Monad
import Data.Either
import Text.Parsec

spec :: Spec
spec = do
    parseVsUnparseSpec
    parseSpec
    unparseSpec
    getStartSpec
    getEndSpec

parseVsUnparseSpec :: Spec
parseVsUnparseSpec = describe "parse vs unparse" $ do
    it "composes parse and unparse into id" $ do
        property $ \astList@(AstList k "" xs) -> do
            parseAstList k (unparseAstList astList) `shouldBe` Right astList
            unparseAstList <$> parseAstList k (unparseAstList astList)
                `shouldBe` Right (unparseAstList astList)
            
parseSpec :: Spec
parseSpec = describe "parse" $ do
    forM_ astKinds $ \k -> do
        let (start, end) = (getStart k, getEnd k)

        it "rejects an empty string" $ do
            isLeft (parseAstList k "") `shouldBe` True

        it "parses empty list" $ do
            parseAstList k [start, end] `shouldBe` Right (AstList k "" [])
            parseAstList k [start, ' ', end] `shouldBe` Right (AstList k "" [])

        it "parses one element" $ do
            parseAstList k [start, '1', end] `shouldBe` Right (AstList k "" [1])
            parseAstList k [start, ' ', '1', end] `shouldBe` Right (AstList k "" [1])
            parseAstList k [start, '1', ' ', end] `shouldBe` Right (AstList k "" [1])
            parseAstList k [start, ' ', '1', ' ', end] `shouldBe` Right (AstList k "" [1])

        it "parses two elements" $ do
            parseAstList k [start, '1', ' ', '2', end] `shouldBe` Right (AstList k "" [1, 2])
            parseAstList k [start, ' ', '1', ' ', '2', end] `shouldBe` Right (AstList k "" [1, 2])
            parseAstList k [start, '1', ' ', '2', ' ', end] `shouldBe` Right (AstList k "" [1, 2])
            parseAstList k [start, ' ', '1', ' ', '2', ' ', end] `shouldBe` Right (AstList k "" [1, 2])

        it "parses n elements" $ do
            property $ \xs -> do
                let s = toAstList k xs
                parseAstList k s `shouldBe` Right (AstList k "" xs)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    forM_ astKinds $ \k -> do
        let (start, end) = (getStart k, getEnd k)

        it "unparses empty brackets for empty elements" $ do
            unparseAstList (AstList k "" []) `shouldBe` [start, end]

        it "unparses with one element" $ do
            unparseAstList (AstList k "" [1]) `shouldBe` [start, '1', end]

        it "unparses with two elements" $ do
            unparseAstList (AstList k "" [1, 2]) `shouldBe` [start, '1', ' ', '2', end]

        it "unparses with n elements" $ do
            property $ \xs -> do
                unparseAstList (AstList k "" (xs :: [Int])) `shouldBe` toAstList k xs

getStartSpec :: Spec
getStartSpec = describe "getStart" $ do
    it "returns start for kind" $ do
        getStart AstKindList `shouldBe` '['
        getStart AstKindDict `shouldBe` '{'
        getStart AstKindStruct `shouldBe` '<'
        getStart AstKindEval `shouldBe` '('

getEndSpec :: Spec
getEndSpec = describe "getEnd" $ do
    it "returns end for kind" $ do
        getEnd AstKindList `shouldBe` ']'
        getEnd AstKindDict `shouldBe` '}'
        getEnd AstKindStruct `shouldBe` '>'
        getEnd AstKindEval `shouldBe` ')'

-- Utils
parseAstList k = parse (parser k doc p) "tests"
unparseAstList = unparse " " show

doc = many space
p = (read :: String -> Int) <$> liftM2 (++) (option "" $ string "-" ) (many1 digit)

toAstList k = replace '[' (getStart k) . replace ']' (getEnd k) . replace ',' ' ' . show

replace _   _   []     = []
replace old new (x:xs) =
    let x' = if x == old then new else x
    in x' : replace old new xs

astKinds = [ AstKindList, AstKindDict, AstKindStruct, AstKindEval ]

instance Arbitrary a => Arbitrary (AstList a) where
    arbitrary = arbitraryOf arbitrary

arbitraryOf a = liftM3 AstList (elements astKinds) (pure "") $ chooseInt (0, 5) >>= flip vectorOf a