module TestUtils where

import BuiltIns
import Control.Monad
import Data.NatSpec
import Test.QuickCheck
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos
import Types

-- Constants
digits = ['0'..'9']
lettersUpper = ['A'..'Z']
lettersLower = ['a'..'z']
symbols = " !#$%&'()*+,-.:;<=>?@[]^`{|}~"
noEscapeChars = digits ++ lettersUpper ++ lettersLower ++ symbols
accentChars = "àâäĉèéêëĝĥîïĵôöŝùûüŵŷÿ"
escapees = "\"\\\b\f\n\r\t"
doubleQuoteChar = '"'
backslashChar = '\\'
solidusChar = '/'
underscore = '_'

validFirsts = underscore : lettersUpper ++ lettersLower ++ accentChars
invalidFirsts = digits ++ symbols ++ escapees
validNexts = underscore : digits ++ lettersUpper ++ lettersLower ++ accentChars

kinds = [ KindList, KindDict, KindForm ]
argPassSymbs = [ symbEval, symbQuote, symbUnquote, symbDeepQuote, symbDeepUnquote ]

-- Functions
parseElem :: String -> Parser Elem
parseElem s = Elem s . (read :: String -> Int) <$> many1 digit

unparseElem :: Elem -> String
unparseElem (Elem s x) = s ++ show x

-- Types and instances
instance Arbitrary Ident where
    arbitrary = Ident <$> arbMany 1 5 (do IdentPart p <- arbitrary; return p)

newtype IdentPart = IdentPart String deriving (Show, Eq)
instance Arbitrary IdentPart where
    arbitrary = IdentPart <$> liftM2 (:) (elements validFirsts) (chooseInt (0, 5) >>= flip vectorOf (elements validNexts))

newtype ArgPassSymb = ArgPassSymb Symb deriving (Show, Eq)
instance Arbitrary ArgPassSymb where
    arbitrary = ArgPassSymb <$> elements argPassSymbs

newtype D = D String deriving (Show, Eq)
instance Arbitrary D where arbitrary = D <$> elements [" ", "\n", "\t", "\r\n", "\v"]
arbD = do D d <- arbitrary; return d

newtype Few a = Few [a] deriving (Show, Eq)
instance Arbitrary a => Arbitrary (Few a) where
    arbitrary = Few <$> arbMany 0 5 arbitrary

data Elem
    = Elem String Int
    deriving (Show, Eq)

instance Arbitrary Elem where
    arbitrary = do
        D d <- arbitrary
        Positive x <- arbitrary
        return $ Elem d x
instance Arbitrary AstExpr where
    arbitrary = chooseInt (0, 3) >>= arbitraryExprOf

arbK = elements kinds
arbMany min max me = chooseInt (min, max) >>= flip vectorOf me

arbitraryExprOf depth = do
    -- Num: 0, Str: 1, Ident: 2, Symb: 3, List: 4
    D d <- arbitrary
    choice <- chooseInt (0, if depth <= 0 then 3 else 4)
    AstExpr pos d <$> case choice of
        0 -> AstNum <$> arbitrary
        1 -> AstStr <$> arbitrary
        2 -> AstIdent <$> arbitrary
        3 -> AstSymb <$> liftM2 Symb arbitrary arbitrary
        4 -> liftM3 AstList arbK arbD (arbMany 0 3 $ arbitraryExprOf $ depth-1)

newtype UnquoteValid
    = UnquoteValid AstExpr
    deriving (Show, Eq)

newtype UnquoteValids
    = UnquoteValids [AstExpr]
    deriving (Show, Eq)

instance Arbitrary UnquoteValid where
    arbitrary = UnquoteValid <$> (chooseInt (0, 3) >>= arbitraryUnquoteValidOf)

instance Arbitrary UnquoteValids where
    arbitrary = UnquoteValids <$> arbMany 0 3 (do UnquoteValid e <- arbitrary; return e)

arbitraryUnquoteValidOf depth = do
    -- Num: 0, Str: 1, Symb: 2, List: 3
    D d <- arbitrary
    choice <- chooseInt (0, if depth <= 0 then 2 else 3)
    AstExpr pos d <$> case choice of
        0 -> AstNum <$> arbitrary
        1 -> AstStr <$> arbitrary
        2 -> AstSymb <$> liftM2 Symb arbitrary arbitrary
        3 -> liftM2 (AstList KindList) arbD $ arbMany 0 3 $ arbitraryUnquoteValidOf $ depth-1