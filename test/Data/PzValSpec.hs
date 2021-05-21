module Data.PzValSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.FuncSpec
import Data.Numb
import Data.Str
import Data.Symb
import Data.PzVal
import Data.WithPos
import Data.WithPosSpec
import TestUtils

spec :: Spec
spec = return ()

-- Utils
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
            , liftM2 PzFunc (arbWithDepth depth) $ arbWithDepth depth
            ]
        )

newtype ArbDict = ArbDict Dict deriving (Show, Eq)
instance Arbitrary ArbDict where arbitrary = arbDepth
instance ArbWithDepth ArbDict where arbWithDepth depth = ArbDict <$> arbWithDepth depth