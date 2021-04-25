module Ast.AstExprSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Ast.AstIdent as I
import qualified Ast.AstIdentSpec as IS
import qualified Ast.AstList as L
import qualified Ast.AstListSpec as LS
import qualified Ast.AstNum as N
import qualified Ast.AstNumSpec as NS
import qualified Ast.AstStr as St
import qualified Ast.AstStrSpec as StS
import qualified Ast.AstSymb as Sy
import qualified Ast.AstSymbSpec as SyS

import Ast.AstExpr
import Ast.AstListSpec (D(..))
import Control.Monad
import Data.Either as Either
import Data.Nat (Nat(..))
import Text.Parsec
import Text.Parsec.Pos

spec :: Spec
spec = do
    parseVsUnparseSpec
    parseSpec
    unparseSpec
    quoteVsUnquoteSpec
    quoteSpec
    unquoteSpec

parseVsUnparseSpec :: Spec
parseVsUnparseSpec = describe "parse vs unparse" $ do
    it "composes parse and unparse into id" $ do
        property $ \e@(AstExpr pos d val) -> do
            parse (spaces >> parser doc d) "tests" (unparse e) `shouldBe` Right e
            unparse <$> parse (spaces >> parser doc d) "tests" (unparse e) `shouldBe` Right (unparse e)

parseSpec :: Spec
parseSpec = describe "parse" $ do
    it "parses num" $ do
        property $ \(D d) n -> do
            parse (parser doc d) "tests" (N.unparse n) `shouldBe` Right (AstExpr pos d $ ValNum n)

    it "parses str" $ do
        property $ \(D d) s -> do
            parse (parser doc d) "tests" (St.unparse s) `shouldBe` Right (AstExpr pos d $ ValStr s)

    it "parses ident" $ do
        property $ \(D d) i -> do
            parse (parser doc d) "tests" (I.unparse i) `shouldBe` Right (AstExpr pos d $ ValIdent i)

    it "parses symb" $ do
        property $ \(D d) s -> do
            parse (parser doc d) "tests" (Sy.unparse s) `shouldBe` Right (AstExpr pos d $ ValSymb s)

    it "parses list" $ do
        property $ \(D d) l -> do
            parse (parser doc d) "tests" (L.unparse unparse l) `shouldBe` Right (AstExpr pos d $ ValList l)

unparseSpec :: Spec
unparseSpec = describe "unparse" $ do
    it "unparses num" $ do
        property $ \(D d) n -> do
            unparse (AstExpr pos d $ ValNum n) `shouldBe` d ++ N.unparse n

    it "unparses str" $ do
        property $ \(D d) s -> do
            unparse (AstExpr pos d $ ValStr s) `shouldBe` d ++ St.unparse s

    it "unparses ident" $ do
        property $ \(D d) i -> do
            unparse (AstExpr pos d $ ValIdent i) `shouldBe` d ++ I.unparse i

    it "unparses symb" $ do
        property $ \(D d) s -> do
            unparse (AstExpr pos d $ ValSymb s) `shouldBe` d ++ Sy.unparse s

    it "unparses list" $ do
        property $ \(D d) l -> do
            unparse (AstExpr pos d $ ValList l) `shouldBe` d ++ L.unparse unparse l

quoteVsUnquoteSpec :: Spec
quoteVsUnquoteSpec = describe "quote vs unquote" $ do
    it "composes quote and unquote into id" $ do
        property $ \e1 (UnquoteValid e2) -> do
            quote <$> unquote (quote e1) `shouldBe` Right (quote e1)
            quote <$> unquote e2 `shouldBe` Right e2

quoteSpec :: Spec
quoteSpec = describe "quote" $ do
    it "converts numbers into themselves" $ do
        property $ \(D d) n -> do
            let e = AstExpr pos d $ ValNum n
            quote e `shouldBe` e

    it "converts strings into themselves" $ do
        property $ \(D d) s -> do
            let e = AstExpr pos d $ ValStr s
            quote e `shouldBe` e

    it "converts identifiers into single-quoted symbols" $ do
        property $ \(D d) i -> do
            quote (AstExpr pos d $ ValIdent i) `shouldBe` AstExpr pos d (ValSymb $ Sy.AstSymb Z i)

    it "converts symbols into one-more-quoted symbols" $ do
        property $ \(D d) s@(Sy.AstSymb n i) -> do
            quote (AstExpr pos d $ ValSymb s) `shouldBe` AstExpr pos d (ValSymb $ Sy.AstSymb (S n) i)

    it "converts lists into 'list-prefixed lists" $ do
        property $ \(D d1) (L.AstList _ d2 es) -> do
            quote (AstExpr pos d1 $ ValList $ L.AstList L.KindList d2 es) `shouldBe`
                AstExpr pos d1 (ValList $ L.AstList L.KindList d2 $ map quote $ identList : es)

    it "converts dicts into 'dict-prefixed lists" $ do
        property $ \(D d1) (L.AstList _ d2 es) -> do
            quote (AstExpr pos d1 $ ValList $ L.AstList L.KindDict d2 es) `shouldBe`
                AstExpr pos d1 (ValList $ L.AstList L.KindList d2 $ map quote $ identDict : es)

    it "converts forms into lists" $ do
        property $ \(D d1) (L.AstList _ d2 es) -> do
            quote (AstExpr pos d1 $ ValList $ L.AstList L.KindForm d2 es) `shouldBe`
                AstExpr pos d1 (ValList $ L.AstList L.KindList d2 $ map quote es)

unquoteSpec :: Spec
unquoteSpec = describe "unquote" $ do
    it "converts numbers into themselves" $ do
        property $ \(D d) n -> do
            let e = AstExpr pos d $ ValNum n
            unquote e `shouldBe` Right e

    it "converts strings into themselves" $ do
        property $ \(D d) s -> do
            let e = AstExpr pos d $ ValStr s
            unquote e `shouldBe` Right e

    it "rejects identifiers" $ do
        property $ \(D d) i -> do
            let e = AstExpr pos d $ ValIdent i
            unquote e `shouldBe` Left ("Unquote: unexpected identifier: " ++ I.unparse i)

    it "converts single-quoted symbols into identifiers" $ do
        property $ \(D d) i -> do
            unquote (AstExpr pos d $ ValSymb $ Sy.AstSymb Z i) `shouldBe` Right (AstExpr pos d $ ValIdent i)

    it "converts two-or-more-quoted symbols into one-less-quoted symbol" $ do
        property $ \(D d) n i -> do
            unquote (AstExpr pos d $ ValSymb $ Sy.AstSymb (S n) i) `shouldBe` Right (AstExpr pos d $ ValSymb $ Sy.AstSymb n i)

    it "converts lists into forms" $ do
        property $ \(D d1) (D d2) (UnquoteValids es) -> do
            let list = L.AstList L.KindList d2 es
                mactual = unquote $ AstExpr pos d1 $ ValList list
            isRight mactual `shouldBe` True
            mactual `shouldBe` (AstExpr pos d1 . ValList . L.AstList L.KindForm d2 <$> mapM unquote es)
    
    it "rejects dictionaries" $ do
        property $ \(D d1) (D d2) es -> do
            let dictionary = L.AstList L.KindDict d2 es
            unquote (AstExpr pos d1 $ ValList dictionary) `shouldBe`
                Left ("Unquote: unexpected dictionary: " ++ L.unparse unparse dictionary)

    it "rejects forms" $ do
        property $ \(D d1) (D d2) es -> do
            let form = L.AstList L.KindForm d2 es
            unquote (AstExpr pos d1 $ ValList form) `shouldBe`
                Left ("Unquote: unexpected form: " ++ L.unparse unparse form)

-- Utils
doc = many space
pos = newPos "" 0 0

instance Arbitrary AstExpr where
    arbitrary = chooseInt (0, 3) >>= arbitraryExprOf

arbitraryExprOf depth = do
    -- Num: 0, Str: 1, Ident: 2, Symb: 3, List: 4
    D d <- arbitrary
    choice <- chooseInt (0, if depth <= 0 then 3 else 4)
    AstExpr pos d <$> case choice of
        0 -> ValNum <$> arbitrary
        1 -> ValStr <$> arbitrary
        2 -> ValIdent <$> arbitrary
        3 -> ValSymb <$> arbitrary
        4 -> ValList <$> LS.arbitraryOf (arbitraryExprOf $ depth-1)

identList = AstExpr pos "" $ ValIdent $ I.AstIdent (I.AstIdentPart 'l' "ist") []
identDict = AstExpr pos "" $ ValIdent $ I.AstIdent (I.AstIdentPart 'd' "ict") []

newtype UnquoteValid
    = UnquoteValid AstExpr
    deriving (Show, Eq)

newtype UnquoteValids
    = UnquoteValids [AstExpr]
    deriving (Show, Eq)

instance Arbitrary UnquoteValid where
    arbitrary = UnquoteValid <$> (chooseInt (0, 3) >>= arbitraryUnquoteValidOf)

instance Arbitrary UnquoteValids where
    arbitrary = do
        depth <- chooseInt (0, 3)
        L.AstList _ _ es <- LS.arbitraryOf $ arbitraryUnquoteValidOf depth
        return $ UnquoteValids es

arbitraryUnquoteValidOf depth = do
    -- Num: 0, Str: 1, Symb: 2, List: 3
    D d <- arbitrary
    choice <- chooseInt (0, if depth <= 0 then 2 else 3)
    AstExpr pos d <$> case choice of
        0 -> ValNum <$> arbitrary
        1 -> ValStr <$> arbitrary
        2 -> ValSymb <$> arbitrary
        3 -> do
            L.AstList _ d es <- LS.arbitraryOf $ arbitraryUnquoteValidOf $ depth-1
            return $ ValList $ L.AstList L.KindList d es