module Ast.AstListSpec where

import Test.Hspec
import Test.QuickCheck

import Ast.AstList
import Control.Monad
import Data.Either
import Data.List
import Text.Parsec
import Text.Parsec.String

spec :: Spec
spec = do
    parseVsUnparseSpec
    parseSpec
    unparseSpec
    parseElemsVsUnparseElemsSpec
    parseElemsSpec
    unparseElemsSpec
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

parseElemsVsUnparseElemsSpec :: Spec
parseElemsVsUnparseElemsSpec = describe "parseElems vs unparseElems" $ do
    let end = char '$'
        parser' = parseElems doc p' end
    it "composes parseElems and unparseElems into id" $ do
        property $ \xs -> do
            let es = map (\x -> (d, x)) (xs :: [Int])
                f = \(s, x) -> s ++ show x
            parse parser' "tests" (unparseElems d f es ++ "$") `shouldBe` Right (es, d)

parseElemsSpec :: Spec
parseElemsSpec = describe "parseElems" $ do
    let end = char '$'
        parser' = parseElems doc p' end
    it "rejects empty string" $ do
        isLeft (parse parser' "tests" "") `shouldBe` True

    it "parses no elems" $ do
        parse parser' "tests" " $" `shouldBe` Right ([], " ")

    it "parses one elem" $ do
        parse parser' "tests" (" 123\n$") `shouldBe` Right ([ (" ", 123) ], "\n")

    it "parses two elems" $ do
        parse parser' "tests" (" 123\n234\t$") `shouldBe` Right ([ (" ", 123), ("\n", 234) ], "\t")

    it "parses three elems" $ do
        parse parser' "tests" (" 123\n234\t345\r\n$") `shouldBe` Right ([ (" ", 123), ("\n", 234), ("\t", 345) ], "\r\n")

    it "parses n elems" $ do
        property $ \xs -> do
            let es = map (\x -> (d, x)) (xs :: [Int])
            parse parser' "tests" (concatMap (\x -> d ++ show x) xs ++ d ++ "$") `shouldBe` Right (es, d)

unparseElemsSpec :: Spec
unparseElemsSpec = describe "unparseElems" $ do
    it "unparses empty list" $ do
        unparseElems d show ([] :: [(String, Int)]) `shouldBe` d

    it "unparses one elem" $ do
        property $ \e -> do
            unparseElems d show ([e] :: [(String, Int)]) `shouldBe` show e ++ d

    it "unparses two elems" $ do
        property $ \e1 e2 -> do
            unparseElems d show ([e1, e2] :: [(String, Int)]) `shouldBe` show e1 ++ show e2 ++ d

    it "unparses three elems" $ do
        property $ \e1 e2 e3 -> do
            unparseElems d show ([e1, e2, e3] :: [(String, Int)]) `shouldBe` show e1 ++ show e2 ++ show e3 ++ d

    it "unparses n elems" $ do
        property $ \es -> do
            unparseElems d show (es :: [(String, Int)]) `shouldBe` concatMap show es ++ d

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
d = " \n\t\r\n\v"

p :: Parser Int
p = (read :: String -> Int) <$> liftM2 (++) (option "" $ string "-" ) (many1 digit)

p' :: String -> Parser (String, Int)
p' s = (\x -> (s,x)) . (read :: String -> Int) <$> liftM2 (++) (option "" $ string "-" ) (many1 digit)

toAstList k = replace '[' (getStart k) . replace ']' (getEnd k) . replace ',' ' ' . show

replace _   _   []     = []
replace old new (x:xs) =
    let x' = if x == old then new else x
    in x' : replace old new xs

astKinds = [ AstKindList, AstKindDict, AstKindStruct, AstKindEval ]

instance Arbitrary a => Arbitrary (AstList a) where
    arbitrary = arbitraryOf arbitrary

arbitraryOf a = liftM3 AstList (elements astKinds) (pure "") $ chooseInt (0, 5) >>= flip vectorOf a