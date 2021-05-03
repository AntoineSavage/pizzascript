module TestUtils where

import qualified Data.Map as M

import BuiltIns
import Control.Monad
import Data.NatSpec
import Test.QuickCheck
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos
import Types
import Utils

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
argPasses = [ Eval, Quote, Unquote, DeepQuote, DeepUnquote ]

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

instance Arbitrary Symb where
    arbitrary = liftM2 Symb arbitrary arbitrary

newtype D = D String deriving (Show, Eq)
instance Arbitrary D where arbitrary = D <$> elements [" ", "\n", "\t", "\r\n", "\v"]
arbD = do D d <- arbitrary; return d

instance Arbitrary SourcePos where arbitrary = liftM3 newPos arbitrary arbitrary arbitrary

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
    p <- arbitrary
    (D d) <- arbitrary
    fmap (AstExpr p d) $ oneof $
        [ AstNum <$> arbitrary
        , AstStr <$> arbitrary
        , AstIdent <$> arbitrary
        , AstSymb <$> liftM2 Symb arbitrary arbitrary
        ] ++
        (if depth <= 0 then [] else
            [ liftM3 AstList arbK arbD (arbMany 0 3 $ arbitraryExprOf $ depth-1)
            ]
        )

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
    p <- arbitrary
    (D d) <- arbitrary
    fmap (AstExpr p d) $ oneof $
        [ AstNum <$> arbitrary
        , AstStr <$> arbitrary
        , AstSymb <$> liftM2 Symb arbitrary arbitrary
        ] ++
        ( if depth <= 0 then [] else
            [ liftM2 (AstList KindList) arbD $ arbMany 0 3 $ arbitraryUnquoteValidOf $ depth-1
            ]
        )

instance Arbitrary PzVal where
    arbitrary = chooseInt (0, 3) >>= arbitraryValOf

arbitraryValOf depth = oneof $
        [ return PzUnit
        , PzNum <$> arbitrary
        , PzStr <$> arbitrary
        , PzSymb <$> liftM2 Symb arbitrary arbitrary
        ] ++
        (if depth <= 0 then [] else
            [ PzList <$> arbMany 0 3 (arbitraryValOf $ depth-1)
            , PzDict . M.fromList <$> arbMany 0 3 (liftM2 (,) (arbitraryValOf $ depth-1) $ arbitraryValOf $ depth-1)
            , PzFunc <$> liftM5 Func arbitrary arbitrary (elements argPasses) arbitrary arbitrary
            ]
        )

instance Arbitrary FuncArgs where
    arbitrary = oneof
        [ ArgsVaria <$> arbitrary
        , ArgsArity <$> arbitrary
        ]

instance Arbitrary FuncBody where
    arbitrary = oneof
        [ BodyBuiltIn <$> arbitrary
        , BodyCustom <$> arbitrary
        ]