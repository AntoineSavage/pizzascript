module Ops.PzValSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Exception
import Control.Monad
import Data.Either
import Ops.FuncSpec
import Ops.Numb
import Ops.NumbSpec
import Ops.Str
import Ops.StrSpec
import Ops.Symb
import Ops.PzVal
import Symbs
import TestUtils
import Text.Parsec
import Text.Parsec.String
import Types.PzVal
import Types.PzValSpec

spec :: Spec
spec = do
    fromQuotedSpec
    unDictKeySpec
    parseValVsUnparseValSpec
    parseValSpec
    unparseValSpec
    parseListVsUnparseListSpec
    parseListSpec
    unparseListSpec
    parseManyVsUnparseManySpec
    parseManySpec
    unparseManySpec

fromQuotedSpec :: Spec
fromQuotedSpec = describe "fromQuoted" $ do
    it "rejects unit" $ do
        let v = PzUnit
        evaluate (fromQuoted v) `shouldThrow` errorCall ("Can only convert quoted values: " ++ show v)

    it "handles numbers as id" $ do
        property $ \n -> do
            fromQuoted (PzNum n) `shouldBe` PzNum n

    it "handles strings as id" $ do
        property $ \s -> do
            fromQuoted (PzStr s) `shouldBe` PzStr s

    it "handles symbols as id" $ do
        property $ \s -> do
            fromQuoted (PzSymb s) `shouldBe` PzSymb s

    it "handles lists as id" $ do
        property $ \(Few xs) -> do
            fromQuoted (PzList xs) `shouldBe` PzList (map fromQuoted xs)

    it "rejects dictionaries" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            evaluate (fromQuoted v) `shouldThrow` errorCall ("Can only convert quoted values: " ++ show v)

    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            evaluate (fromQuoted v) `shouldThrow` errorCall ("Can only convert quoted values: " ++ show v)

unDictKeySpec :: Spec
unDictKeySpec = describe "unDictKey" $ do
    it "composes with DictKey into id" $ do
        property $ \v -> do
            unDictKey (DictKey v) `shouldBe` v

parseValVsUnparseValSpec :: Spec
parseValVsUnparseValSpec = describe "parseVal vs unparseVal" $ do
    it "composes parseVal and unparseVal into id" $ do
        let f v last  = unparseVal f v ++ if last then "" else " "
        property $ \v -> do
            let s = unparseVal f v
            parse pv "tests" s `shouldBe` Right v
            unparseVal f <$> parse pv "tests" s `shouldBe` Right s

parseValSpec :: Spec
parseValSpec = describe "parseVal" $ do
    it "rejects empty string" $ do
        leftAsStr (parse (parseVal ignore undefined) "tests" "") `shouldContain`
            "expecting number, string, symbol (or identifier) or list (or dictionary or function)"

    it "parses num" $ do
        property $ \n -> do
            parse (parseVal ignore undefined) "tests" (unparseNumb n) `shouldBe` Right (PzNum n)

    it "parses str" $ do
        property $ \s -> do
            parse (parseVal ignore undefined) "tests" (unparseStr s) `shouldBe` Right (PzStr s)

    it "parses symb" $ do
        property $ \s -> do
            parse (parseVal ignore undefined) "tests" (unparseSymb s) `shouldBe` Right (PzSymb s)

    it "parses list" $ do
        let f v last  = unparseVal f v ++ if last then "" else " "
        property $ \(Few xs) -> do
            parse (parseVal ignore pv) "tests" (unparseList pzSymbList pzSymbDict f xs) `shouldBe` Right (PzList xs)

unparseValSpec :: Spec
unparseValSpec = describe "unparseVal" $ do
    it "rejects unit" $ do
        let v = PzUnit
        evaluate (unparseVal undefined v) `shouldThrow` errorCall ("Can only unparse quoted values: " ++ show v)

    it "rejects dict" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            evaluate (unparseVal undefined v) `shouldThrow` errorCall ("Can only unparse quoted values: " ++ show v)

    it "rejects func" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            evaluate (unparseVal undefined v) `shouldThrow` errorCall ("Can only unparse quoted values: " ++ show v)

    it "unparses num" $ do
        property $ \n -> do
            unparseVal undefined (PzNum n) `shouldBe` unparseNumb n

    it "unparses str" $ do
        property $ \s -> do
            unparseVal undefined (PzStr s) `shouldBe` unparseStr s

    it "unparses symb" $ do
        property $ \s -> do
            unparseVal undefined (PzSymb s) `shouldBe` unparseSymb s

    it "unparses list" $ do
        let f v last  = unparseVal f v ++ if last then "" else " "
        property $ \(Few xs) -> do
            unparseVal f (PzList xs) `shouldBe` unparseList pzSymbList pzSymbDict f xs

parseListVsUnparseListSpec :: Spec
parseListVsUnparseListSpec = describe "parseList vs unparsesList" $ do
    it "composes parseList and unparseList into id" $ do
        property $ \(Few es) -> do
            let les = ple : es
                des = pde : es
                sl = unparseList' les
                sd = unparseList' des
                sf = unparseList' es
            parseList' sl `shouldBe` Right les
            parseList' sd `shouldBe` Right des
            parseList' sf `shouldBe` Right es
            unparseList' <$> parseList' sl `shouldBe` Right sl
            unparseList' <$> parseList' sd `shouldBe` Right sd
            unparseList' <$> parseList' sf `shouldBe` Right sf

parseListSpec :: Spec
parseListSpec = describe "parseList" $ do
    it "rejects empty string" $ do
        isLeft (parseList' "") `shouldBe` True

    it "parses zero elements" $ do
        parseList' "[]" `shouldBe` Right [ple]
        parseList' "{}" `shouldBe` Right [pde]
        parseList' "()" `shouldBe` Right []

    it "parses one element" $ do
        property $ \e -> do
            parseList' ("[" ++ str e ++ "]") `shouldBe` Right [ple, e]
            parseList' ("{" ++ str e ++ "}") `shouldBe` Right [pde, e]
            parseList' ("(" ++ str e ++ ")") `shouldBe` Right [e]

    it "parses two elements" $ do
        property $ \e1 e2 -> do
            parseList' ("[" ++ str e1 ++ str e2 ++ "]") `shouldBe` Right [ple, e1, e2]
            parseList' ("{" ++ str e1 ++ str e2 ++ "}") `shouldBe` Right [pde, e1, e2]
            parseList' ("(" ++ str e1 ++ str e2 ++ ")") `shouldBe` Right [e1, e2]

    it "parses three elements" $ do
        property $ \e1 e2 e3 -> do
            parseList' ("[" ++ str e1 ++ str e2 ++ str e3 ++ "]") `shouldBe` Right [ple, e1, e2, e3]
            parseList' ("{" ++ str e1 ++ str e2 ++ str e3 ++ "}") `shouldBe` Right [pde, e1, e2, e3]
            parseList' ("(" ++ str e1 ++ str e2 ++ str e3 ++ ")") `shouldBe` Right [e1, e2, e3]

    it "parses N elements" $ do
        property $ \es -> do
            parseList' ("[" ++ concatMap str es ++ "]") `shouldBe` Right (ple : es)
            parseList' ("{" ++ concatMap str es ++ "}") `shouldBe` Right (pde : es)
            parseList' ("(" ++ concatMap str es ++ ")") `shouldBe` Right (es)

unparseListSpec :: Spec
unparseListSpec = describe "unparseList" $ do
    it "unparses empty list into empty form" $ do
        unparseList' [] `shouldBe` "()"
  
    it "unparses list" $ do
        property $ \(Few es) -> do
            let elems = ple : es
            unparseList' elems `shouldBe` "[" ++ untrail (concatMap str es) ++ "]"
  
    it "unparses di dict" $ do
        property $ \(Few es) -> do
            let elems = pde : es
            unparseList' elems `shouldBe` "{" ++ untrail (concatMap str es) ++ "}"
  
    it "unparses form" $ do
        property $ \(Few es) -> do
            unparseList' es `shouldBe` "(" ++ untrail (concatMap str es) ++ ")"

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

    it "unparses one elem" $ do
        property $ \e -> do
            unparseMany' [e] `shouldBe` str' e

    it "unparses two elems" $ do
        property $ \e1 e2 -> do
            unparseMany' [e1, e2] `shouldBe` str e1 ++ str' e2

    it "unparses three elems" $ do
        property $ \e1 e2 e3 -> do
            unparseMany' [e1, e2, e3] `shouldBe` str e1 ++ str e2 ++ str' e3

    it "unparses n elems" $ do
        property $ \(Few es) -> do
            unparseMany' es `shouldBe` untrail (concatMap str es)

-- Utils
pv = parseVal ignore pv

ignore = spaces
str x = unparseElem x False
str' x = unparseElem x True
untrail s = case reverse s of
    (' ':s') -> reverse s'
    _ -> s

parseList' = parse (parseList ple pde ignore parseElem) "tests"
unparseList' = unparseList ple pde unparseElem

parseMany' = parse (parseMany spaces parseElem $ void $ char '$') "tests"
unparseMany' es = unparseMany unparseElem es

-- Test-only types
newtype Elem = Elem Int deriving (Show, Eq)
instance Arbitrary Elem where arbitrary = do Positive x <- arbitrary; return $ Elem x

ple :: Elem
ple = Elem $ -1

pde :: Elem
pde = Elem $ -2

parseElem :: Parser Elem
parseElem = Elem . read <$> many1 digit

unparseElem :: Elem -> Bool -> String
unparseElem (Elem x) last = show x ++ if last then "" else " "