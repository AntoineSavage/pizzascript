module TestUtils where

import qualified Data.Map as M

import BuiltIns
import Control.Monad
import Data.List
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

argPassSymbs = [ symbEval, symbQuote, symbUnquote, symbDeepQuote, symbDeepUnquote ]

-- Functions
parseElem :: Parser Elem
parseElem = Elem . read <$> many1 digit

unparseElem :: Elem -> String
unparseElem (Elem x) = show x

-- Types and instances
instance Arbitrary Ident where arbitrary = Ident <$> arbMany 1 5 arbIdentPart
instance ArbWithDepth Ident where arbWithDepth _ = arbitrary
arbIdentPart = liftM2 (:) (elements validFirsts) $ chooseInt (0, 5) >>= flip vectorOf (elements validNexts)

instance Arbitrary Symb where arbitrary = liftM2 Symb arbitrary arbitrary
instance Arbitrary SourcePos where arbitrary = liftM3 newPos arbitrary arbitrary arbitrary

instance ArbWithDepth a => Arbitrary (WithPos a) where arbitrary = arbDepth
instance ArbWithDepth a => ArbWithDepth (WithPos a) where arbWithDepth = liftM2 WithPos arbitrary . arbWithDepth

instance Arbitrary AstExpr where arbitrary = arbDepth
instance ArbWithDepth AstExpr where
    arbWithDepth depth = oneof $
        [ AstNum <$> arbitrary
        , AstStr <$> arbitrary
        , AstIdent <$> arbitrary
        , AstSymb <$> liftM2 Symb arbitrary arbitrary
        ] ++
        (if depth <= 0 then [] else
            [ liftM2 AstList arbitrary $ arbFew $ arbWithDepth $ depth-1
            ]
        )

instance Arbitrary AstListKind where arbitrary = elements [ KindList, KindDict, KindForm ]

instance Arbitrary PzVal where arbitrary = arbDepth
instance ArbWithDepth PzVal where
    arbWithDepth depth = oneof $
        [ return PzUnit
        , PzNum <$> arbitrary
        , PzStr <$> arbitrary
        , PzSymb <$> liftM2 Symb arbitrary arbitrary
        ] ++
        (if depth <= 0 then [] else
            [ fmap PzList $ arbFew $ arbWithDepth $ depth-1
            , fmap PzDict $ arbDict depth
            , PzFunc <$> liftM4 Func (arbDict depth) arbitrary arbitrary arbitrary
            ]
        )

instance Arbitrary FuncImpureArgs where 
    arbitrary = oneof
        [ return None
        , liftM2 ArgPass arbitrary arbitrary
        , liftM3 Both arbitrary arbitrary arbitrary
        ]

instance Arbitrary ArgPass where arbitrary = elements [ Eval, Quote, Unquote, DeepQuote, DeepUnquote ]
instance ArbWithDepth ArgPass where arbWithDepth _ = arbitrary

instance Arbitrary FuncArgs where arbitrary = oneof [ArgsVaria <$> arbitrary, ArgsArity <$> arbFew arbitrary]
instance Arbitrary FuncBody where arbitrary = oneof [BodyBuiltIn <$> arbitrary, BodyCustom <$> arbFew arbitrary]

-- Test-only types
class Arbitrary a => ArbWithDepth a where arbWithDepth :: Int -> Gen a

newtype Elem = Elem Int deriving (Show, Eq)
instance Arbitrary Elem where arbitrary = do Positive x <- arbitrary; return $ Elem x

newtype Few a = Few [a] deriving (Show, Eq)
instance Arbitrary a => Arbitrary (Few a) where arbitrary = Few <$> arbFew arbitrary

newtype Uniques a = Uniques [a] deriving (Show, Eq)
instance (Eq a, Arbitrary a) => Arbitrary (Uniques a) where arbitrary = Uniques . nub <$> arbMany 1 10 arbitrary

newtype ArbDict = ArbDict Dict deriving (Show, Eq)
instance Arbitrary ArbDict where arbitrary = arbDepth
instance ArbWithDepth ArbDict where arbWithDepth depth = ArbDict <$> arbDict depth
arbDict depth = let sub = arbWithDepth $ depth-1 in M.fromList <$> arbFew (liftM2 (,) sub sub)

newtype UnquoteValid = UnquoteValid (WithPos AstExpr) deriving (Show, Eq)
instance Arbitrary UnquoteValid where arbitrary = arbDepth
instance ArbWithDepth UnquoteValid where
    arbWithDepth depth = fmap UnquoteValid $ liftM2 WithPos arbitrary $ oneof $
        [ AstNum <$> arbitrary
        , AstStr <$> arbitrary
        , AstSymb <$> liftM2 Symb arbitrary arbitrary
        ] ++
        ( if depth <= 0 then [] else
            [ AstList KindList <$> arbFew arbUnquoteValid
            ]
        )

newtype UnquoteValids = UnquoteValids [WithPos AstExpr] deriving (Show, Eq)
instance Arbitrary UnquoteValids where arbitrary = UnquoteValids <$> arbFew arbUnquoteValid

-- Arbitrary utils
arbDepth :: ArbWithDepth a => Gen a
arbDepth = chooseInt (0, 3) >>= arbWithDepth

arbFew :: Gen a -> Gen [a]
arbFew = arbMany 0 4

arbMany :: Int -> Int -> Gen a -> Gen [a]
arbMany min max me = chooseInt (min, max) >>= flip vectorOf me

arbUnquoteValid :: Gen (WithPos AstExpr)
arbUnquoteValid = do UnquoteValid e <- arbitrary; return e
