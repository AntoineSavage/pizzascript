module Ast.AstExprSpec where

import Test.Hspec
import Test.QuickCheck

import Ast.AstExpr
import qualified Ast.AstIdentSpec as AstIdentSpec
import qualified Ast.AstIdent as AstIdent
import qualified Ast.AstListSpec as AstListSpec
import qualified Ast.AstList as AstList
import qualified Ast.AstNumSpec as AstNumSpec
import qualified Ast.AstNum as AstNum
import qualified Ast.AstStrSpec as AstStrSpec
import qualified Ast.AstStr as AstStr
import qualified Ast.AstSymbSpec as AstSymbSpec
import qualified Ast.AstSymb as AstSymb

import Control.Monad
import Data.Either
import Text.Parsec

spec :: Spec
spec = do
    parseVsUnparseSpec
    parseSpec
    ignoreSpec
    unparseSpec

parseVsUnparseSpec :: Spec
parseVsUnparseSpec = describe "parse vs unparse" $ do
    it "composes parse and unparse into id" $ do
        property $ \e -> do
            parse parser "tests" (unparse sep e) `shouldBe` Right e
            unparse sep <$> parse parser "tests" (unparse sep e) `shouldBe` Right (unparse sep e)

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "parses num" $ do
        property $ \n -> do
            parse parser "tests" (AstNum.unparse n) `shouldBe` Right (AstNum n)

    it "parses str" $ do
        property $ \s -> do
            parse parser "tests" (AstStr.unparse s) `shouldBe` Right (AstStr s)

    it "parses ident" $ do
        property $ \i -> do
            parse parser "tests" (AstIdent.unparse i) `shouldBe` Right (AstIdent i)

    it "parses symb" $ do
        property $ \s -> do
            parse parser "tests" (AstSymb.unparse s) `shouldBe` Right (AstSymb s)

    it "parses list" $ do
        property $ \l -> do
            parse parser "tests" (AstList.unparse sep (unparse sep) l) `shouldBe` Right (AstList l)

ignoreSpec :: Spec
ignoreSpec = describe "ignore" $ do
    it "parses empty string" $ do
        isRight (parse ignore "tests" "") `shouldBe` True

    it "parses unprintable string" $ do
        isRight (parse ignore "tests" "\0\1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\32\127") `shouldBe` True

    it "parses single comment (lf)" $ do
        isRight (parse ignore "tests" "# # 123 \" a ' [ ( ] ) { < } > \n") `shouldBe` True

    it "parses single comment (crlf)" $ do
        isRight (parse ignore "tests" "# # 123 \" a ' [ ( ] ) { < } > \r\n") `shouldBe` True

    it "parses single comment (eof)" $ do
        isRight (parse ignore "tests" "# # 123 \" a ' [ ( ] ) { < } >") `shouldBe` True

    it "parses whitespace and comments" $ do
        isRight (parse ignore "tests" (
            " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >\n" ++
            " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >\r\n" ++
            " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >"
            )) `shouldBe` True

    it "stops at/rejects non-whitespace, non-comment" $ do
        isRight (parse ignore "tests" "123") `shouldBe` True
        isLeft (parse (ignore >> eof) "tests" "123") `shouldBe` True

        isRight (parse ignore "tests" "\"") `shouldBe` True
        isLeft (parse (ignore >> eof) "tests" "\"") `shouldBe` True

        isRight (parse ignore "tests" "a") `shouldBe` True
        isLeft (parse (ignore >> eof) "tests" "a") `shouldBe` True

        isRight (parse ignore "tests" "'") `shouldBe` True
        isLeft (parse (ignore >> eof) "tests" "'") `shouldBe` True

        isRight (parse ignore "tests" "[") `shouldBe` True
        isLeft (parse (ignore >> eof) "tests" "[") `shouldBe` True

        isRight (parse ignore "tests" "{") `shouldBe` True
        isLeft (parse (ignore >> eof) "tests" "{") `shouldBe` True

        isRight (parse ignore "tests" "<") `shouldBe` True
        isLeft (parse (ignore >> eof) "tests" "<") `shouldBe` True

        isRight (parse ignore "tests" "(") `shouldBe` True
        isLeft (parse (ignore >> eof) "tests" "(") `shouldBe` True

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses num" $ do
        property $ \n -> do
            unparse undefined (AstNum n) `shouldBe` AstNum.unparse n

    it "unparses str" $ do
        property $ \s -> do
            unparse undefined (AstStr s) `shouldBe` AstStr.unparse s

    it "unparses ident" $ do
        property $ \i -> do
            unparse undefined (AstIdent i) `shouldBe` AstIdent.unparse i

    it "unparses symb" $ do
        property $ \s -> do
            unparse undefined (AstSymb s) `shouldBe` AstSymb.unparse s

    it "unparses list" $ do
        property $ \l -> do
            unparse sep (AstList l) `shouldBe` AstList.unparse sep (unparse sep) l

-- Utils
sep = " \n\r\n\t\v # comment\n"

instance Arbitrary AstExpr where
    arbitrary = chooseInt (0, 3) >>= arbitraryOf

arbitraryOf d = do
    -- Num: 0, Str: 1, Ident: 2, Symb: 3, List: 4
    choice <- chooseInt (0, if d <= 0 then 3 else 4)
    case choice of
        0 -> AstNum <$> arbitrary
        1 -> AstStr <$> arbitrary
        2 -> AstIdent <$> arbitrary
        3 -> AstSymb <$> arbitrary
        4 -> AstList <$> AstListSpec.arbitraryOf (arbitraryOf $ d-1)

