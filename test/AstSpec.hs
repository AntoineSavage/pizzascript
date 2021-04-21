module AstSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Ast.AstExprSpec as AstExprSpec
import qualified Ast.AstExpr as AstExpr
import qualified Ast.AstList as AstList

import Ast
import Ast.AstListSpec (D(..))
import Control.Monad
import Data.Either
import Data.List
import Text.Parsec

spec :: Spec
spec = do
    docSpec
    parseVsUnparseSpec
    parseSpec
    unparseSpec

docSpec :: Spec
docSpec = describe "doc" $ do
    it "parses empty string" $ do
        isRight (parse doc "tests" "") `shouldBe` True

    it "parses unprintable string" $ do
        isRight (parse doc "tests" "\0\1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\32\127") `shouldBe` True

    it "parses single comment (lf)" $ do
        isRight (parse doc "tests" "# # 123 \" a ' [ ( ] ) { < } > \n") `shouldBe` True

    it "parses single comment (crlf)" $ do
        isRight (parse doc "tests" "# # 123 \" a ' [ ( ] ) { < } > \r\n") `shouldBe` True

    it "parses single comment (eof)" $ do
        isRight (parse doc "tests" "# # 123 \" a ' [ ( ] ) { < } >") `shouldBe` True

    it "parses whitespace and comments" $ do
        isRight (parse doc "tests" (
            " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >\n" ++
            " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >\r\n" ++
            " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >"
            )) `shouldBe` True

    it "stops at/rejects non-whitespace, non-comment" $ do
        isRight (parse doc "tests" "123") `shouldBe` True
        isLeft (parse (doc >> eof) "tests" "123") `shouldBe` True

        isRight (parse doc "tests" "\"") `shouldBe` True
        isLeft (parse (doc >> eof) "tests" "\"") `shouldBe` True

        isRight (parse doc "tests" "a") `shouldBe` True
        isLeft (parse (doc >> eof) "tests" "a") `shouldBe` True

        isRight (parse doc "tests" "'") `shouldBe` True
        isLeft (parse (doc >> eof) "tests" "'") `shouldBe` True

        isRight (parse doc "tests" "[") `shouldBe` True
        isLeft (parse (doc >> eof) "tests" "[") `shouldBe` True

        isRight (parse doc "tests" "{") `shouldBe` True
        isLeft (parse (doc >> eof) "tests" "{") `shouldBe` True

        isRight (parse doc "tests" "<") `shouldBe` True
        isLeft (parse (doc >> eof) "tests" "<") `shouldBe` True

        isRight (parse doc "tests" "(") `shouldBe` True
        isLeft (parse (doc >> eof) "tests" "(") `shouldBe` True


parseVsUnparseSpec :: Spec
parseVsUnparseSpec = describe "parse vs unparse" $ do
    it "composes parse and unparse into id" $ do
        property $ \ast@(Ast d es) -> do
            parse parser "tests" (unparse ast) `shouldBe` Right ast
            unparse <$> parse parser "tests" (unparse ast) `shouldBe` Right (unparse ast)

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "parses empty string" $ do
        parse parser "tests" "" `shouldBe` Right (Ast "" [])

    it "parses no expressions" $ do
        property $ \(D d) ->
            parse parser "tests" d `shouldBe` Right (Ast d [])

    it "parses single expression" $ do
        property $ \(D d) e -> do
            parse parser "tests" (AstExpr.unparse e ++ d) `shouldBe` Right (Ast d [e])

    it "parses two expressions" $ do
        property $ \(D d) e1 e2 -> do
            parse parser "tests" (AstExpr.unparse e1 ++ AstExpr.unparse e2 ++ d) `shouldBe` Right (Ast d [e1, e2])

    it "parses three expressions" $ do
        property $ \(D d) e1 e2 e3 -> do
            parse parser "tests" (AstExpr.unparse e1 ++ AstExpr.unparse e2 ++ AstExpr.unparse e3 ++ d) `shouldBe` Right (Ast d [e1, e2, e3])

    it "parses n expressions" $ do
        property $ \(Ast d es) -> do
            parse parser "tests" (concatMap AstExpr.unparse es ++ d) `shouldBe` Right (Ast d es)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses no expressions" $ do
        property $ \(D d) ->
            unparse (Ast d []) `shouldBe` d

    it "unparses single expression" $ do
        property $ \(D d) e -> do
            unparse (Ast d [e]) `shouldBe` AstExpr.unparse e ++ d

    it "unparses two expressions" $ do
        property $ \(D d) e1 e2 -> do
            unparse (Ast d [e1, e2])`shouldBe` AstExpr.unparse e1 ++ AstExpr.unparse e2 ++ d

    it "unparses three expressions" $ do
        property $ \(D d) e1 e2 e3 -> do
            unparse (Ast d [e1, e2, e3]) `shouldBe` AstExpr.unparse e1 ++ AstExpr.unparse e2 ++ AstExpr.unparse e3 ++ d

    it "unparses n expressions" $ do
        property $ \(D d) es -> do
            unparse (Ast d es) `shouldBe` (concatMap AstExpr.unparse es ++ d)

-- Utils
instance Arbitrary Ast where
    arbitrary = Ast " " <$> (chooseInt (0, 5) >>= vector)