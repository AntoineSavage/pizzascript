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
    forM_ astKinds $ \k -> do
        it "composes parse and unparse into id" $ do
            property $ \(D d) es -> do
                let astList = AstList k d es
                    s = unparse' astList
                parse' k s `shouldBe` Right astList
                unparse' <$> parse' k s `shouldBe` Right s
            
parseSpec :: Spec
parseSpec = describe "parse" $ do
    forM_ astKinds $ \k -> do
        let (start, end) = (getStart k, getEnd k)

        it "rejects an empty string" $ do
            isLeft (parse' k "") `shouldBe` True

        it "parses no elems" $ do
            property $ \(D d) -> 
                parse' k ([start] ++ d ++ [end]) `shouldBe` Right (AstList k d [])

        it "parses one elem" $ do
            property $ \(D d) e -> 
                parse' k ([start] ++ unparseElem e ++ d ++ [end]) `shouldBe` Right (AstList k d [e])

        it "parses two elems" $ do
            property $ \(D d) e1 e2 -> 
                parse' k ([start] ++ unparseElem e1 ++ unparseElem e2 ++ d ++ [end]) `shouldBe` Right (AstList k d [e1, e2])

        it "parses three elems" $ do
            property $ \(D d) e1 e2 e3 -> 
                parse' k ([start] ++ unparseElem e1 ++ unparseElem e2 ++ unparseElem e3 ++ d ++ [end]) `shouldBe` Right (AstList k d [e1, e2, e3])

        it "parses n elems" $ do
            property $ \(D d) es -> 
                parse' k ([start] ++ concatMap unparseElem es ++ d ++ [end]) `shouldBe` Right (AstList k d es)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    forM_ astKinds $ \k -> do
        let (start, end) = (getStart k, getEnd k)

        it "unparses zero elems" $ do
            property $ \(D d) -> do
                unparse' (AstList k d []) `shouldBe` [start] ++ d ++ [end]

        it "unparses one elem" $ do
            property $ \(D d) e -> do
                unparse' (AstList k d [e]) `shouldBe` [start] ++ unparseElem e ++ d ++ [end]

        it "unparses two elems" $ do
            property $ \(D d) e1 e2 -> do
                unparse' (AstList k d [e1, e2]) `shouldBe` [start] ++ unparseElem e1 ++ unparseElem e2 ++ d ++ [end]

        it "unparses three elems" $ do
            property $ \(D d) e1 e2 e3  -> do
                unparse' (AstList k d [e1, e2, e3]) `shouldBe` [start] ++ unparseElem e1 ++ unparseElem e2 ++ unparseElem e3 ++ d ++ [end]

        it "unparses n elems" $ do
            property $ \(D d) es  -> do
                unparse' (AstList k d es) `shouldBe` [start] ++ concatMap unparseElem es ++ d ++ [end]

parseElemsVsUnparseElemsSpec :: Spec
parseElemsVsUnparseElemsSpec = describe "parseElems vs unparseElems" $ do
    it "composes parseElems and unparseElems into id" $ do
        property $ \(D d) es -> do
            parseElems' (unparseElems' d es ++ "$") `shouldBe` Right (es, d)

parseElemsSpec :: Spec
parseElemsSpec = describe "parseElems" $ do
    it "rejects empty string" $ do
        isLeft (parseElems' "") `shouldBe` True

    it "parses no elems" $ do
        property $ \(D d) -> do
            parseElems' (d ++ "$") `shouldBe` Right ([], d)

    it "parses one elem" $ do
        property $ \(D d) e -> do
            parseElems' (unparseElem e ++ d ++ "$") `shouldBe` Right ([e], d)

    it "parses two elems" $ do
        property $ \(D d) e1 e2 -> do
            parseElems' (unparseElem e1 ++ unparseElem e2 ++ d ++ "$") `shouldBe` Right ([e1, e2], d)

    it "parses three elems" $ do
        property $ \(D d) e1 e2 e3 -> do
            parseElems' (unparseElem e1 ++ unparseElem e2 ++ unparseElem e3 ++ d ++ "$") `shouldBe` Right ([e1, e2, e3], d)

    it "parses n elems" $ do
        property $ \(D d) es -> do
            parseElems' (concatMap unparseElem es ++ d ++ "$") `shouldBe` Right (es, d)

unparseElemsSpec :: Spec
unparseElemsSpec = describe "unparseElems" $ do
    it "unparses empty list" $ do
        property $ \(D d) -> do
            unparseElems' d [] `shouldBe` d

    it "unparses one elem" $ do
        property $ \(D d) e -> do
            unparseElems' d [e] `shouldBe` unparseElem e ++ d

    it "unparses two elems" $ do
        property $ \(D d) e1 e2 -> do
            unparseElems' d [e1, e2] `shouldBe` unparseElem e1 ++ unparseElem e2 ++ d

    it "unparses three elems" $ do
        property $ \(D d) e1 e2 e3 -> do
            unparseElems' d [e1, e2, e3] `shouldBe` unparseElem e1 ++ unparseElem e2 ++ unparseElem e3 ++ d

    it "unparses n elems" $ do
        property $ \(D d) es -> do
            unparseElems' d es `shouldBe` concatMap unparseElem es ++ d

getStartSpec :: Spec
getStartSpec = describe "getStart" $ do
    it "returns start for kind" $ do
        getStart KindList `shouldBe` '['
        getStart KindDict `shouldBe` '{'
        getStart KindForm `shouldBe` '('

getEndSpec :: Spec
getEndSpec = describe "getEnd" $ do
    it "returns end for kind" $ do
        getEnd KindList `shouldBe` ']'
        getEnd KindDict `shouldBe` '}'
        getEnd KindForm `shouldBe` ')'

-- Utils
data Elem = Elem String Int deriving (Show, Eq)

parseElem :: String -> Parser Elem
parseElem s = Elem s . (read :: String -> Int) <$> many1 digit

unparseElem :: Elem -> String
unparseElem (Elem s x) = s ++ show x

parse' k = parse (parser k doc parseElem) "tests"
unparse' = unparse unparseElem

parseElems' = parse (parseElems doc parseElem end) "tests"
unparseElems' d ps = unparseElems d unparseElem ps

end = void $ char '$'
doc = many space

astKinds = [ KindList, KindDict, KindForm ]
ds = [" ", "\n", "\t", "\r\n", "\v"]

newtype D = D String deriving (Show, Eq)
instance Arbitrary D where arbitrary = D <$> elements ds
arbD = do D d <- arbitrary; return d

instance Arbitrary Elem where
    arbitrary = do
        D d <- arbitrary
        Positive x <- arbitrary
        return $ Elem d x

instance Arbitrary a => Arbitrary (AstList a) where
    arbitrary = arbitraryOf arbitrary

arbitraryOf a = liftM3 AstList (elements astKinds) arbD $ chooseInt (0, 5) >>= flip vectorOf a