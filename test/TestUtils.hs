{-# LANGUAGE LambdaCase #-}
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

kinds = [ KindList, KindDict, KindForm ]
argPasses = [ Eval, Quote, Unquote, DeepQuote, DeepUnquote ]

-- Functions
parseElem :: Parser Elem
parseElem = Elem . read <$> many1 digit

unparseElem :: Maybe Elem -> String
unparseElem = \case
    Nothing -> ""
    Just (Elem x) -> show x ++ " "

-- Types and instances
instance Arbitrary Ident where arbitrary = Ident <$> arbMany 1 5 arbIdentPart
instance ArbWithDepth Ident where arbWithDepth _ = arbitrary

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
            [ fmap PzList $ arbWithDepth depth
            , fmap PzDict $ arbWithDepth depth
            , PzFunc <$> arbWithDepth depth
            ]
        )

instance Arbitrary Func where arbitrary = arbDepth
instance ArbWithDepth Func where
    arbWithDepth depth = liftM4 Func (arbWithDepth depth) arbitrary arbitrary (arbWithDepth depth)

instance Arbitrary FuncImpureArgs where 
    arbitrary = oneof
        [ return None
        , liftM2 ArgPass arbitrary arbitrary
        , liftM3 Both arbitrary arbitrary arbIdentUnqual
        ]

instance Arbitrary ArgPass where arbitrary = elements argPasses
instance ArbWithDepth ArgPass where arbWithDepth _ = arbitrary

instance Arbitrary FuncArgs where arbitrary = oneof [ArgsVaria <$> arbIdentUnqual, liftM2 ArgsArity arbitrary $ arbFew arbIdentUnqual]
instance Arbitrary FuncBody where arbitrary = arbDepth
instance ArbWithDepth FuncBody where arbWithDepth depth = oneof [BodyBuiltIn <$> arbitrary, BodyCustom <$> arbFew (arbWithDepth depth)]

instance Arbitrary FuncCustom where arbitrary = arbDepth
instance ArbWithDepth FuncCustom where
    arbWithDepth depth = do
        let getImpArgsIdents (Both _ _ i) = [i]
            getImpArgsIdents _            = []

            getArgsIdents (ArgsVaria i)    = [i]
            getArgsIdents (ArgsArity _ is) = is

        impArgs <- arbitrary
        args <- arbitrary
        let is = getImpArgsIdents impArgs ++ getArgsIdents args
        if is == nub is
            then fmap (FuncCustom impArgs args) $ arbWithDepth depth
            else arbWithDepth depth

instance Arbitrary StackFrame where arbitrary = arbDepth
instance ArbWithDepth StackFrame where
    arbWithDepth depth = oneof
        [liftM2 Block arbDepth arbDepth
        , liftM4 Form arbDepth arbitrary arbitrary arbDepth
        , do 
            a <- arbDepth
            b <- arbitrary
            c <- arbDepth
            d <- arbDepth
            e <- arbDepth
            f <- arbDepth
            return $ Invoc a b c d e f
        ]

-- Test-only types
newtype ValidCodepoint = ValidCodepoint Int deriving (Show, Eq)
instance Arbitrary ValidCodepoint where arbitrary = ValidCodepoint <$> chooseInt (0, 0x10FFFF)

newtype InvalidCodepoint = InvalidCodepoint Int deriving (Show, Eq)
instance Arbitrary InvalidCodepoint where arbitrary = InvalidCodepoint <$> chooseInt (0x110000, maxBound)

newtype IdentPart = IdentPart String deriving (Show, Eq)
instance Arbitrary IdentPart where arbitrary = fmap IdentPart $ liftM2 (:) (elements validFirsts) $ chooseInt (0, 5) >>= flip vectorOf (elements validNexts)
arbIdentPart = do IdentPart s <- arbitrary; return s

class Arbitrary a => ArbWithDepth a where arbWithDepth :: Int -> Gen a
instance ArbWithDepth a => ArbWithDepth (Maybe a) where arbWithDepth depth = oneof [return Nothing, Just <$> arbWithDepth depth]

newtype Elem = Elem Int deriving (Show, Eq)
instance Arbitrary Elem where arbitrary = do Positive x <- arbitrary; return $ Elem x

newtype Few a = Few [a] deriving (Show, Eq)
instance Arbitrary a => Arbitrary (Few a) where arbitrary = Few <$> arbFew arbitrary

newtype Uniques a = Uniques [a] deriving (Show, Eq)
instance (Eq a, Arbitrary a) => Arbitrary (Uniques a) where arbitrary = Uniques . nub <$> arbMany 1 10 arbitrary

newtype ArbDict = ArbDict Dict deriving (Show, Eq)
instance Arbitrary ArbDict where arbitrary = arbDepth
instance ArbWithDepth ArbDict where arbWithDepth depth = ArbDict <$> arbWithDepth depth

instance ArbWithDepth a => ArbWithDepth [a] where arbWithDepth depth = arbFew $ arbWithDepth $ depth-1
instance (Ord k, ArbWithDepth k, ArbWithDepth v) => ArbWithDepth (M.Map k v) where
    arbWithDepth depth =
        let sub :: ArbWithDepth a => Gen a
            sub = arbWithDepth $ depth-1
        in fmap M.fromList $ arbFew $ liftM2 (,) sub sub

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

newtype PzFalsish = PzFalsish (WithPos PzVal) deriving (Show, Eq)
instance Arbitrary PzFalsish where
    arbitrary = do
        p <- arbitrary
        fmap PzFalsish $ elements $ map (WithPos p) $
            [ PzUnit
            , PzNum 0
            , PzStr ""
            , PzList []
            , PzDict M.empty
            ]

newtype PzTruish = PzTruish (WithPos PzVal) deriving (Show, Eq)
instance Arbitrary PzTruish where
    arbitrary = fmap PzTruish $ liftM2 WithPos arbitrary $ oneof
            [ PzNum . getNonZero <$> arbitrary
            , PzStr . getNonEmpty <$> arbitrary
            , PzList  <$> liftM2 (:) arbDepth (arbFew arbDepth)
            , fmap PzDict $ liftM3 M.insert arbDepth arbDepth arbDepth
            , PzFunc <$> arbitrary
            ]

-- Arbitrary utils
arbDepth :: ArbWithDepth a => Gen a
arbDepth = chooseInt (0, 3) >>= arbWithDepth

arbFew :: Gen a -> Gen [a]
arbFew = arbMany 0 4

arbMany :: Int -> Int -> Gen a -> Gen [a]
arbMany min max me = chooseInt (min, max) >>= flip vectorOf me

arbUnquoteValid :: Gen (WithPos AstExpr)
arbUnquoteValid = do UnquoteValid e <- arbitrary; return e

arbIdentUnqual :: Gen (WithPos Ident)
arbIdentUnqual = liftM2 WithPos arbitrary $ Ident . (:[]) <$> arbIdentPart