{-# LANGUAGE LambdaCase #-}
module Data.LstSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Either
import Data.Lst
import TestUtils
import Text.Parsec
import Text.Parsec.String

spec :: Spec
spec = do
    getStartSpec
    getEndSpec
    parseLstVsUnparseLstSpec
    parseLstSpec
    unparseLstSpec
    parseManyVsUnparseManySpec
    parseManySpec
    unparseManySpec

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

parseLstVsUnparseLstSpec :: Spec
parseLstVsUnparseLstSpec = describe "parseLst vs unparseLst" $ do
    forM_ kinds_ $ \k -> do
        it "composes parseLst and unparseLst into id" $ do
            property $ \(Few es) -> do
                let s = unparseLst' k es
                    unLst (Lst _ xs) = xs
                parseLst' s `shouldBe` Right (Lst k es)
                unparseLst' k . unLst <$> parseLst' s `shouldBe` Right s
           
parseLstSpec :: Spec
parseLstSpec = describe "parseLst" $ do
    forM_ kinds_ $ \k -> do
        let (start, end) = (getStart k, getEnd k)

        it "rejects an empty string" $ do
            isLeft (parseLst' "") `shouldBe` True

        it "parses no elems" $ do
            parseLst' ([start] ++ [end]) `shouldBe` Right (Lst k [])

        it "parses one elem" $ do
            property $ \e ->
                parseLst' ([start] ++ str e ++ [end]) `shouldBe` Right (Lst k [e])

        it "parses two elems" $ do
            property $ \e1 e2 ->
                parseLst' ([start] ++ str e1 ++ str e2 ++ [end]) `shouldBe` Right (Lst k [e1, e2])

        it "parses three elems" $ do
            property $ \e1 e2 e3 ->
                parseLst' ([start] ++ str e1 ++ str e2 ++ str e3 ++ [end]) `shouldBe` Right (Lst k [e1, e2, e3])

        it "parses n elems" $ do
            property $ \(Few es) ->
                parseLst' ([start] ++ concatMap str es ++ [end]) `shouldBe` Right (Lst k es)

unparseLstSpec :: Spec
unparseLstSpec = describe "unparseLst" $ do
    forM_ kinds_ $ \k -> do
        let (start, end) = (getStart k, getEnd k)

        it "unparses zero elems" $ do
            unparseLst' k [] `shouldBe` [start] ++ [end]

        it "unparses one elem" $ do
            property $ \e -> do
                unparseLst' k [e] `shouldBe` [start] ++ str e ++ [end]

        it "unparses two elems" $ do
            property $ \e1 e2 -> do
                unparseLst' k [e1, e2] `shouldBe` [start] ++ str e1 ++ str e2 ++ [end]

        it "unparses three elems" $ do
            property $ \e1 e2 e3  -> do
                unparseLst' k [e1, e2, e3] `shouldBe` [start] ++ str e1 ++ str e2 ++ str e3 ++ [end]

        it "unparses n elems" $ do
            property $ \es  -> do
                unparseLst' k es `shouldBe` [start] ++ concatMap str es ++ [end]

parseManyVsUnparseManySpec :: Spec
parseManyVsUnparseManySpec = describe "parseMany vs unparseMany" $ do
    it "composes parseMany and unparseMany into id" $ do
        property $ \(Few es) -> do
            parseMany' (unparseMany' es ++ "$") `shouldBe` Right es

parseManySpec :: Spec
parseManySpec = describe "parseMany" $ do
    it "rejects empty string" $ do
        isLeft (parseMany' "") `shouldBe` True

    it "parses no elems" $ do
        parseMany' ("$") `shouldBe` Right []

    it "parses no elems with whitespace" $ do
        parseMany' "   $" `shouldBe` Right []

    it "parses one elem" $ do
        property $ \e -> do
            parseMany' (str e ++ "$") `shouldBe` Right [e]

    it "parses two elems" $ do
        property $ \e1 e2 -> do
            parseMany' (str e1 ++ str e2 ++ "$") `shouldBe` Right [e1, e2]

    it "parses three elems" $ do
        property $ \e1 e2 e3 -> do
            parseMany' (str e1 ++ str e2 ++ str e3 ++ "$") `shouldBe` Right [e1, e2, e3]

    it "parses n elems" $ do
        property $ \(Few es) -> do
            parseMany' (concatMap str es ++ "$") `shouldBe` Right es

unparseManySpec :: Spec
unparseManySpec = describe "unparseMany" $ do
    it "unparses empty list" $ do
        unparseMany' [] `shouldBe` ""
        unparseMany' [] `shouldBe` unparseElem_ Nothing

    it "unparses one elem" $ do
        property $ \e -> do
            unparseMany' [e] `shouldBe` str e

    it "unparses two elems" $ do
        property $ \e1 e2 -> do
            unparseMany' [e1, e2] `shouldBe` str e1 ++ str e2

    it "unparses three elems" $ do
        property $ \e1 e2 e3 -> do
            unparseMany' [e1, e2, e3] `shouldBe` str e1 ++ str e2 ++ str e3

    it "unparses n elems" $ do
        property $ \(Few es) -> do
            unparseMany' es `shouldBe` concatMap str es

-- Utils
kinds_ = [ KindList, KindDict, KindForm ]

ignore :: Parser ()
ignore = spaces

-- TODO Remove _ once TestUtils is cleaned-up
newtype Elem_ = Elem_ Int deriving (Show, Eq)
instance Arbitrary Elem_ where arbitrary = do Positive x <- arbitrary; return $ Elem_ x

parseElem_ :: Parser Elem_
parseElem_ = Elem_ . read <$> many1 digit

unparseElem_ :: Maybe Elem_ -> String
unparseElem_ = \case
    Nothing -> ""
    Just (Elem_ x) -> show x ++ " "

str = unparseElem_.Just

parseLst' = parse (parseLst ignore parseElem_) "tests"
unparseLst' k es = unparseLst unparseElem_ (Lst k es)

parseMany' = parse (parseMany spaces parseElem_ $ void $ char '$') "tests"
unparseMany' es = unparseMany unparseElem_ es