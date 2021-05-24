module Data.PzValSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.FuncSpec
import Data.Numb
import Data.NumbSpec
import Data.Str
import Data.StrSpec
import Data.Symb
import Data.SymbSpec
import Data.PzVal
import TestUtils

spec :: Spec
spec = return ()

-- Utils
instance Arbitrary PzVal where arbitrary = arbDepth
instance ArbWithDepth PzVal where
    arbWithDepth depth = oneof $
        [ return PzUnit
        , PzNum <$> arbitrary
        , PzStr <$> arbitrary
        , PzSymb <$> arbitrary
        ] ++
        (if depth <= 0 then [] else
            [ fmap PzList $ arbWithDepth depth
            , fmap PzDict $ arbWithDepth depth
            , liftM2 PzFunc (arbWithDepth depth) $ arbWithDepth depth
            ]
        )

newtype ArbDict = ArbDict Dict deriving (Show, Eq)
instance Arbitrary ArbDict where arbitrary = arbDepth
instance ArbWithDepth ArbDict where arbWithDepth depth = ArbDict <$> arbWithDepth depth

{-


import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.AstExpr
import Data.Ident
import Data.Lst
import Data.LstSpec
import Data.NatSpec
import Data.Numb
import Data.NumbSpec
import Data.Str
import Data.StrSpec
import Data.Symb
import Data.Str
import TestUtils
import Text.Parsec

spec :: Spec
spec = do
    parseExprVsUnparseExprSpec
    parseExprSpec
    unparseExprSpec

parseExprVsUnparseExprSpec :: Spec
parseExprVsUnparseExprSpec = describe "parseExpr vs unparseExpr" $ do
    it "composes parseExpr and unparseExpr into id" $ do
        let f Nothing = ""; f (Just e) = unparseExpr f e ++ " "
            g = parseExpr ignore g
        property $ \e -> do
            let s = unparseExpr f e
            parse g "tests" s `shouldBe` Right e
            unparseExpr f <$> parse g "tests" s `shouldBe` Right s

parseExprSpec :: Spec
parseExprSpec = describe "parseExpr" $ do
    it "parses num" $ do
        property $ \n -> do
            parse (parseExpr ignore undefined) "tests" (unparseNumb n) `shouldBe` Right (AstNum n)

    it "parses str" $ do
        property $ \s -> do
            parse (parseExpr ignore undefined) "tests" (unparseStr s) `shouldBe` Right (AstStr s)

    it "parses ident" $ do
        property $ \ident -> do
            parse (parseExpr ignore undefined) "tests" (unparseIdent ident) `shouldBe` Right (AstIdent ident)

    it "parses symb" $ do
        property $ \n ident -> do
            parse (parseExpr ignore undefined) "tests" (unparseSymb $ Symb n ident) `shouldBe` Right (AstSymb $ Symb n ident)

    it "parses list" $ do
        let f Nothing = ""; f (Just e) = unparseExpr f e ++ " "
            g = parseExpr ignore g
        property $ \(Few es) -> do
            forM_ kinds $ \k -> do
                parse g "tests" (unparseLst f $ Lst k es) `shouldBe` Right (AstList $ Lst k es)

unparseExprSpec :: Spec
unparseExprSpec = describe "unparseExpr" $ do
    it "unparses num" $ do
        property $ \n -> do
            unparseExpr undefined (AstNum $ n) `shouldBe` unparseNumb n

    it "unparses str" $ do
        property $ \s -> do
            unparseExpr undefined (AstStr s) `shouldBe` unparseStr s

    it "unparses ident" $ do
        property $ \ident -> do
            unparseExpr undefined (AstIdent ident) `shouldBe` unparseIdent ident

    it "unparses symb" $ do
        property $ \n ident -> do
            unparseExpr undefined (AstSymb $ Symb n ident) `shouldBe` unparseSymb (Symb n ident)

    it "unparses list" $ do
        let f Nothing = ""; f (Just e) = unparseExpr f e
        property $ \(Few es) -> do
            forM_ kinds $ \k -> do
                unparseExpr f (AstList $ Lst k es) `shouldBe` unparseLst f (Lst k es)

-- Utils
ignore = spaces

instance Arbitrary AstExpr where arbitrary = arbDepth
instance ArbWithDepth AstExpr where
    arbWithDepth depth = oneof $
        [ AstNum . Numb <$> arbitrary
        , AstStr . Str <$> arbitrary
        , AstIdent <$> arbitrary
        , AstSymb <$> liftM2 Symb arbitrary arbitrary
        ] ++
        (if depth <= 0 then [] else
            [ fmap AstList $ liftM2 Lst arbitrary $ arbFew $ arbWithDepth $ depth-1
            ]
        )











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
    forM_ kinds $ \k -> do
        it "composes parseLst and unparseLst into id" $ do
            property $ \(Few es) -> do
                let s = unparseLst' k es
                    unLst (Lst _ xs) = xs
                parseLst' s `shouldBe` Right (Lst k es)
                unparseLst' k . unLst <$> parseLst' s `shouldBe` Right s
           
parseLstSpec :: Spec
parseLstSpec = describe "parseLst" $ do
    forM_ kinds $ \k -> do
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
    forM_ kinds $ \k -> do
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
        unparseMany' [] `shouldBe` unparseElem Nothing

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
kinds = [ KindList, KindDict, KindForm ]
instance Arbitrary LstKind where arbitrary = elements kinds

newtype Elem = Elem Int deriving (Show, Eq)
instance Arbitrary Elem where arbitrary = do Positive x <- arbitrary; return $ Elem x

parseElem :: Parser Elem
parseElem = Elem . read <$> many1 digit

unparseElem :: Maybe Elem -> String
unparseElem = \case
    Nothing -> ""
    Just (Elem x) -> show x ++ " "

str = unparseElem.Just

parseLst' = parse (parseLst spaces parseElem) "tests"
unparseLst' k es = unparseLst unparseElem (Lst k es)

parseMany' = parse (parseMany spaces parseElem $ void $ char '$') "tests"
unparseMany' es = unparseMany unparseElem es

-}