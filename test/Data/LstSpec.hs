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

parseMany' = parse (parseMany spaces parseElem_ $ void $ char '$') "tests"
unparseMany' es = unparseMany unparseElem_ es