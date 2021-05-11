module Ast.AstListSpec where

import Test.Hspec
import Test.QuickCheck

import Ast
import Control.Monad
import Data.Either
import TestUtils
import Text.Parsec
import Types

spec :: Spec
spec = do
    parseListVsUnparseListSpec
    parseListSpec
    unparseListSpec
    getListStartSpec
    getListEndSpec
    parseManyVsUnparseManySpec
    parseManySpec
    unparseManySpec

parseListVsUnparseListSpec :: Spec
parseListVsUnparseListSpec = describe "parseList vs unparseList" $ do
    forM_ kinds $ \k -> do
        it "composes parseList and unparseList into id" $ do
            property $ \(Few es) -> do
                let s = unparseList' k es
                parseList' k s `shouldBe` Right es
                unparseList' k <$> parseList' k s `shouldBe` Right s
           
parseListSpec :: Spec
parseListSpec = describe "parseList" $ do
    forM_ kinds $ \k -> do
        let (start, end) = (getListStart k, getListEnd k)

        it "rejects an empty string" $ do
            isLeft (parseList' k "") `shouldBe` True

        it "parses no elems" $ do
            parseList' k ([start] ++ [end]) `shouldBe` Right []

        it "parses one elem" $ do
            property $ \e ->
                parseList' k ([start] ++ unparseElem' e ++ [end]) `shouldBe` Right [e]

        it "parses two elems" $ do
            property $ \e1 e2 ->
                parseList' k ([start] ++ unparseElem' e1 ++ unparseElem' e2 ++ [end]) `shouldBe` Right [e1, e2]

        it "parses three elems" $ do
            property $ \e1 e2 e3 ->
                parseList' k ([start] ++ unparseElem' e1 ++ unparseElem' e2 ++ unparseElem' e3 ++ [end]) `shouldBe` Right [e1, e2, e3]

        it "parses n elems" $ do
            property $ \(Few es) ->
                parseList' k ([start] ++ concatMap unparseElem' es ++ [end]) `shouldBe` Right es

unparseListSpec :: Spec
unparseListSpec = describe "unparseList" $ do
    forM_ kinds $ \k -> do
        let (start, end) = (getListStart k, getListEnd k)

        it "unparses zero elems" $ do
            unparseList' k [] `shouldBe` [start] ++ [end]

        it "unparses one elem" $ do
            property $ \e -> do
                unparseList' k [e] `shouldBe` [start] ++ unparseElem' e ++ [end]

        it "unparses two elems" $ do
            property $ \e1 e2 -> do
                unparseList' k [e1, e2] `shouldBe` [start] ++ unparseElem' e1 ++ unparseElem' e2 ++ [end]

        it "unparses three elems" $ do
            property $ \e1 e2 e3  -> do
                unparseList' k [e1, e2, e3] `shouldBe` [start] ++ unparseElem' e1 ++ unparseElem' e2 ++ unparseElem' e3 ++ [end]

        it "unparses n elems" $ do
            property $ \es  -> do
                unparseList' k es `shouldBe` [start] ++ concatMap unparseElem' es ++ [end]

getListStartSpec :: Spec
getListStartSpec = describe "getListStart" $ do
    it "returns start for kind" $ do
        getListStart KindList `shouldBe` '['
        getListStart KindDict `shouldBe` '{'
        getListStart KindForm `shouldBe` '('

getListEndSpec :: Spec
getListEndSpec = describe "getListEnd" $ do
    it "returns end for kind" $ do
        getListEnd KindList `shouldBe` ']'
        getListEnd KindDict `shouldBe` '}'
        getListEnd KindForm `shouldBe` ')'

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
            parseMany' (unparseElem' e ++ "$") `shouldBe` Right [e]

    it "parses two elems" $ do
        property $ \e1 e2 -> do
            parseMany' (unparseElem' e1 ++ unparseElem' e2 ++ "$") `shouldBe` Right [e1, e2]

    it "parses three elems" $ do
        property $ \e1 e2 e3 -> do
            parseMany' (unparseElem' e1 ++ unparseElem' e2 ++ unparseElem' e3 ++ "$") `shouldBe` Right [e1, e2, e3]

    it "parses n elems" $ do
        property $ \(Few es) -> do
            parseMany' (concatMap unparseElem' es ++ "$") `shouldBe` Right es

unparseManySpec :: Spec
unparseManySpec = describe "unparseMany" $ do
    it "unparses empty list" $ do
        unparseMany' [] `shouldBe` ""
        unparseMany' [] `shouldBe` unparseElem Nothing

    it "unparses one elem" $ do
        property $ \e -> do
            unparseMany' [e] `shouldBe` unparseElem' e

    it "unparses two elems" $ do
        property $ \e1 e2 -> do
            unparseMany' [e1, e2] `shouldBe` unparseElem' e1 ++ unparseElem' e2

    it "unparses three elems" $ do
        property $ \e1 e2 e3 -> do
            unparseMany' [e1, e2, e3] `shouldBe` unparseElem' e1 ++ unparseElem' e2 ++ unparseElem' e3

    it "unparses n elems" $ do
        property $ \(Few es) -> do
            unparseMany' es `shouldBe` concatMap unparseElem' es

unparseElem' = unparseElem.Just

parseList' k = parse (parseList k ignore parseElem) "tests"
unparseList' k = unparseList k unparseElem

parseMany' = parse (parseMany spaces parseElem $ void $ char '$') "tests"
unparseMany' es = unparseMany unparseElem es