module Data.AtomSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Atom as Atom
import Data.Either
import Text.Parsec
import Utils

spec :: Spec
spec = do
  parseVsUnparseSpec
  unparseSpec
  parserSpec
  nameParserSpec
  fromNameSpec
  toNameSpec

parseVsUnparseSpec :: Spec
parseVsUnparseSpec = do
  describe "unparse vs parse" $ do
    it "unparse then parse returns same atom" $ do
      property $ \(AtomNameArb first nexts) -> do
        forM_ [AtomNone, AtomFalse, AtomTrue, Atom $ first : nexts] $ \atom -> do
          parse parser "tests" (unparse atom) `shouldBe` Right atom

    it "parse then unparse returns same str" $ do
      property $ \(AtomNameArb first nexts) -> do
        forM_ [AtomNone, AtomFalse, AtomTrue, Atom $ first : nexts] $ \atom -> do
          let str = ':' : first : nexts
          unparse <$> parse parser "tests" str `shouldBe` Right str


unparseSpec :: Spec
unparseSpec = do
  describe "unparse" $ do
    it "returns expected name" $ do
      unparse AtomNone `shouldBe` ":none"
      unparse AtomFalse `shouldBe` ":false"
      unparse AtomTrue `shouldBe` ":true"
      unparse (Atom "err") `shouldBe` ":err"
      unparse (Atom "ok") `shouldBe` ":ok"

    it "returns expected name (prop)" $ do
      property $ \(AtomNameArb first nexts) -> do
        let name = first:nexts
        unparse (Atom name) `shouldBe` ':':name


parserSpec :: Spec
parserSpec = do
  describe "parser" $ do
    it "fails if empty string" $ do
      isLeft(parse parser "tests" "") `shouldBe` True

    it "fails if not colon" $ do
      property $ \(AtomNameArb first _) -> do
        isLeft(parse parser "tests" [first]) `shouldBe` True

    it "fails if colon only" $ do
        isLeft(parse nameParser "tests" ":") `shouldBe` True

    it "fails if colon followed by invalid char" $ do
      property $ \(InvalidAtomFirstChar c) (AtomNameArb first nexts) -> do
        isLeft(parse nameParser "tests" (':':c:first:nexts)) `shouldBe` True

    it "succeeds with valid chars" $ do
        parse parser "tests" ":none" `shouldBe` Right AtomNone
        parse parser "tests" ":false" `shouldBe` Right AtomFalse
        parse parser "tests" ":true" `shouldBe` Right AtomTrue
        parse parser "tests" ":err" `shouldBe` Right (Atom "err")
        parse parser "tests" ":ok" `shouldBe` Right (Atom "ok")

    it "succeeds with valid chars followed by invalid char" $ do
      property $ \(AtomNameArb first nexts) (InvalidAtomNextChar  c) -> do
        let name = '_':first:nexts
        parse parser "tests" (':':name ++ [c]) `shouldBe` Right (Atom name)


nameParserSpec :: Spec
nameParserSpec = do
  describe "nameParser" $ do
    it "fails if empty string" $ do
      isLeft(parse nameParser "tests" "") `shouldBe` True

    it "fails if starts with invalid char" $ do
      property $ \(InvalidAtomFirstChar c) (AtomNameArb first nexts) -> do
        isLeft(parse nameParser "tests" (c:first:nexts)) `shouldBe` True

    it "succeeds with valid chars followed by invalid char" $ do
      property $ \(AtomNameArb first nexts) (InvalidAtomNextChar  c) -> do
        let name = first:nexts
        parse nameParser "tests" (name ++ [c]) `shouldBe` Right name

fromNameSpec :: Spec
fromNameSpec = do
  describe "fromName" $ do
    it "returns the atom" $ do
      Atom.fromName "none" `shouldBe `AtomNone
      Atom.fromName "false" `shouldBe `AtomFalse
      Atom.fromName "true" `shouldBe `AtomTrue
      Atom.fromName "err" `shouldBe `(Atom "err")
      Atom.fromName "ok" `shouldBe `(Atom "ok")
      Atom.fromName "nothing" `shouldBe `(Atom "nothing")

    it "returns the atom (prop)" $ do
      property $ \(AtomNameArb first nexts) -> do
        forM_ ["none", "false", "true", "_"] $ \prefix -> do
          let name = prefix ++ [first] ++ nexts
          Atom.fromName name `shouldBe` Atom name

toNameSpec :: Spec
toNameSpec = do
  describe "toName" $ do
    it "returns the atom name" $ do
      Atom.toName AtomNone `shouldBe` "none"
      Atom.toName AtomFalse `shouldBe` "false"
      Atom.toName AtomTrue `shouldBe` "true"
      Atom.toName (Atom "err") `shouldBe` "err"
      Atom.toName (Atom "ok") `shouldBe` "ok"

    it "returns the atom name (prop)" $ do
      property $ \(AtomNameArb first nexts) -> do
        Atom.toName (Atom $ first : nexts) `shouldBe` (first : nexts)

data AtomNameArb = AtomNameArb Char String deriving (Show)
instance Arbitrary AtomNameArb where
  arbitrary = liftM2 AtomNameArb
    (elements atomNameFirsts)
    (sized $ flip vectorOf $ elements atomNameNexts)

newtype InvalidAtomFirstChar = InvalidAtomFirstChar Char deriving (Show)
instance Arbitrary InvalidAtomFirstChar where
  arbitrary = InvalidAtomFirstChar <$> elements invalidFirstChars

newtype InvalidAtomNextChar = InvalidAtomNextChar Char deriving (Show)
instance Arbitrary InvalidAtomNextChar where
  arbitrary = InvalidAtomNextChar <$> elements invalidNextChars

invalidNextChars = "!\"#$%&'()*+-./:;<=>?@[\\]^`{|}~"
invalidFirstChars = digits ++ invalidNextChars