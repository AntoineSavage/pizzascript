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
parseElem :: Parser Elem
parseElem = Elem . read <$> many1 digit

unparseElem :: Elem -> String
unparseElem (Elem x) = show x

-- Arbitrary utils
class Arbitrary a => ArbWithDepth a where arbWithDepth :: Int -> Gen a

arbDepth :: ArbWithDepth a => Gen a
arbDepth = chooseInt (0, 3) >>= arbWithDepth

arbMany :: Int -> Int -> Gen a -> Gen [a]
arbMany min max me = chooseInt (min, max) >>= flip vectorOf me

arbUnquoteValid :: Gen (WithPos AstExpr)
arbUnquoteValid = do UnquoteValid e <- arbitrary; return e

-- Types and instances

instance Arbitrary Ident where arbitrary = Ident <$> arbMany 1 5 arbP
instance ArbWithDepth Ident where arbWithDepth _ = arbitrary

newtype IdentPart = IdentPart String deriving (Show, Eq)
instance Arbitrary IdentPart where
    arbitrary = IdentPart <$> liftM2 (:) (elements validFirsts) (chooseInt (0, 5) >>= flip vectorOf (elements validNexts))
arbP = do IdentPart p <- arbitrary; return p

instance Arbitrary Symb where arbitrary = liftM2 Symb arbitrary arbitrary
instance Arbitrary SourcePos where arbitrary = liftM3 newPos arbitrary arbitrary arbitrary

newtype Elem = Elem Int deriving (Show, Eq)
instance Arbitrary Elem where arbitrary = do Positive x <- arbitrary; return $ Elem x

newtype Few a = Few [a] deriving (Show, Eq)
instance Arbitrary a => Arbitrary (Few a) where arbitrary = Few <$> arbMany 0 5 arbitrary

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
            [ liftM2 AstList (elements kinds) $ arbMany 0 3 $ arbWithDepth $ depth-1
            ]
        )

newtype UnquoteValid = UnquoteValid (WithPos AstExpr) deriving (Show, Eq)
instance Arbitrary UnquoteValid where arbitrary = arbDepth
instance ArbWithDepth UnquoteValid where
    arbWithDepth depth = fmap UnquoteValid $ liftM2 WithPos arbitrary $ oneof $
        [ AstNum <$> arbitrary
        , AstStr <$> arbitrary
        , AstSymb <$> liftM2 Symb arbitrary arbitrary
        ] ++
        ( if depth <= 0 then [] else
            [ AstList KindList <$> arbMany 0 3 arbUnquoteValid
            ]
        )

newtype UnquoteValids = UnquoteValids [WithPos AstExpr] deriving (Show, Eq)
instance Arbitrary UnquoteValids where arbitrary = UnquoteValids <$> arbMany 0 3 arbUnquoteValid

instance Arbitrary PzVal where arbitrary = arbDepth
instance ArbWithDepth PzVal where
    arbWithDepth depth = let sub = arbWithDepth $ depth-1 in oneof $
        [ return PzUnit
        , PzNum <$> arbitrary
        , PzStr <$> arbitrary
        , PzSymb <$> liftM2 Symb arbitrary arbitrary
        ] ++
        (if depth <= 0 then [] else
            [ fmap PzList $ arbMany 0 3 sub
            , fmap (PzDict . M.fromList) $ arbMany 0 3 $ liftM2 (,) sub sub
            , PzFunc <$> liftM4 Func arbitrary arbitrary arbitrary arbitrary
            ]
        )

instance Arbitrary FuncImpureArgs where
    arbitrary = oneof
        [ return None
        , liftM2 ArgPass arbitrary arbitrary
        , liftM3 Both arbitrary arbitrary arbitrary
        ]

instance Arbitrary ArgPass where arbitrary = elements argPasses
instance ArbWithDepth ArgPass where arbWithDepth _ = arbitrary

instance Arbitrary FuncArgs where arbitrary = oneof [ArgsVaria <$> arbitrary, ArgsArity <$> arbitrary]
instance Arbitrary FuncBody where arbitrary = oneof [BodyBuiltIn <$> arbitrary, BodyCustom <$> arbitrary]