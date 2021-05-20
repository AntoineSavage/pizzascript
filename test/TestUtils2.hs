{-# LANGUAGE LambdaCase #-}
module TestUtils2 where
-- TODO This should be moved to the corresponding modules

import qualified Data.Map as M

import Control.Monad
import Data.ArgPass
import Data.AstExprSpec
import Data.FuncArgs
import Data.FuncBody
import Data.FuncImpureArgs
import Data.IdentSpec
import Data.List
import Data.Numb
import Data.Str
import Data.Symb
import Data.WithPos
import Data.WithPosSpec
import Test.QuickCheck
import Types
import Utils
import Utils.ArbWithDepth

-- Constants
argPasses = [ Eval, Quote, Unquote, DeepQuote, DeepUnquote ]

-- Types and instances
instance Arbitrary PzVal where arbitrary = arbDepth
instance ArbWithDepth PzVal where
    arbWithDepth depth = oneof $
        [ return PzUnit
        , PzNum . Numb <$> arbitrary
        , PzStr . Str <$> arbitrary
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
        , liftM3 Both arbitrary arbitrary arbitrary
        ]

instance Arbitrary ArgPass where arbitrary = elements argPasses
instance ArbWithDepth ArgPass where arbWithDepth _ = arbitrary

instance Arbitrary FuncArgs where arbitrary = oneof [ArgsVaria <$> arbitrary, liftM2 ArgsArity arbitrary $ arbFew arbitrary]
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
newtype ArbDict = ArbDict Dict deriving (Show, Eq)
instance Arbitrary ArbDict where arbitrary = arbDepth
instance ArbWithDepth ArbDict where arbWithDepth depth = ArbDict <$> arbWithDepth depth

newtype PzFalsish = PzFalsish (WithPos PzVal) deriving (Show, Eq)
instance Arbitrary PzFalsish where
    arbitrary = do
        p <- arbitrary
        fmap PzFalsish $ elements $ map (WithPos p) $
            [ PzUnit
            , PzNum $ Numb 0
            , PzStr $ Str ""
            , PzList []
            , PzDict M.empty
            ]

newtype PzTruish = PzTruish (WithPos PzVal) deriving (Show, Eq)
instance Arbitrary PzTruish where
    arbitrary = fmap PzTruish $ liftM2 WithPos arbitrary $ oneof
            [ PzNum . Numb . getNonZero <$> arbitrary
            , PzStr . Str . getNonEmpty <$> arbitrary
            , PzList  <$> liftM2 (:) arbDepth (arbFew arbDepth)
            , fmap PzDict $ liftM3 M.insert arbDepth arbDepth arbDepth
            , PzFunc <$> arbitrary
            ]