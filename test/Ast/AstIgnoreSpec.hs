module Ast.AstIgnoreSpec where

import Test.Hspec
import Test.QuickCheck

import Ast
import Data.Either
import Text.Parsec

spec :: Spec
spec = do
    ignoreSpec
    commentSpec

ignoreSpec :: Spec
ignoreSpec = describe "ignore" $ do
    it "parses empty string" $ do
        parse ignore "tests" "" `shouldBe` Right ()

    it "parses unprintable string" $ do
        let s = "\0\1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\32\127"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses comment hash (lf)" $ do
        let s = "#\n"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses comment hash (crlf)" $ do
        let s = "#\r\n"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses comment hash (eof)" $ do
        let s = "#"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses single comment (lf)" $ do
        let s = "# # 123 \" a ' [ ( ] ) { < } > \n"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses single comment (crlf)" $ do
        let s = "# # 123 \" a ' [ ( ] ) { < } > \r\n"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses single comment (eof)" $ do
        let s = "# # 123 \" a ' [ ( ] ) { < } >"
        parse ignore "tests" s `shouldBe` Right ()

    it "parses whitespace and comments" $ do
        let s =     " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >\n"
                ++  " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >\r\n"
                ++  " \n\t\r\n\v# # 123 \" a ' [ ( ] ) { < } >"
        parse ignore "tests" s `shouldBe` Right ()

    it "stops at/rejects non-whitespace, non-comment" $ do
        parse ignore "tests" "123" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "123") `shouldBe` True

        parse ignore "tests" "\"" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "\"") `shouldBe` True

        parse ignore "tests" "a" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "a") `shouldBe` True

        parse ignore "tests" "'" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "'") `shouldBe` True

        parse ignore "tests" "[" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "[") `shouldBe` True

        parse ignore "tests" "{" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "{") `shouldBe` True

        parse ignore "tests" "(" `shouldBe` Right ()
        isLeft (parse (ignore >> eof) "tests" "(") `shouldBe` True

commentSpec :: Spec
commentSpec = describe "comment" $ do
    it "rejects empty string" $ do
        isLeft (parse comment "tests" "") `shouldBe` True

    it "rejects unprintable string" $ do
        let s = "\0\1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\32\127"
        isLeft (parse comment "tests" s) `shouldBe` True

    it "rejects whitespace" $ do
        let s = " \n\t\r\n\v"
        isLeft (parse comment "tests" s) `shouldBe` True

    it "rejects non-comment" $ do
        isLeft (parse comment "tests" "123") `shouldBe` True
        isLeft (parse comment "tests" "\"") `shouldBe` True
        isLeft (parse comment "tests" "a") `shouldBe` True
        isLeft (parse comment "tests" "'") `shouldBe` True
        isLeft (parse comment "tests" "[") `shouldBe` True
        isLeft (parse comment "tests" "{") `shouldBe` True
        isLeft (parse comment "tests" "(") `shouldBe` True

    it "parses comment hash (lf)" $ do
        let s = "#\n"
        parse comment "tests" s `shouldBe` Right ()

    it "parses comment hash (crlf)" $ do
        let s = "#\r\n"
        parse comment "tests" s `shouldBe` Right ()

    it "parses comment hash (eof)" $ do
        let s = "#"
        parse comment "tests" s `shouldBe` Right ()

    it "parses single comment (lf)" $ do
        let s = "# # 123 \" a ' [ ( ] ) { < } > \n"
        parse comment "tests" s `shouldBe` Right ()

    it "parses single comment (crlf)" $ do
        let s = "# # 123 \" a ' [ ( ] ) { < } > \r\n"
        parse comment "tests" s `shouldBe` Right ()

    it "parses single comment (eof)" $ do
        let s = "# # 123 \" a ' [ ( ] ) { < } >"
        parse comment "tests" s `shouldBe` Right ()