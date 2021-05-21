module TestUtils where

import qualified Data.Map as M

import Control.Monad
import Data.List
import Test.QuickCheck

-- Arbitrary constraints
newtype Few a = Few [a] deriving (Show, Eq)
instance Arbitrary a => Arbitrary (Few a) where arbitrary = Few <$> arbFew arbitrary

newtype Uniques a = Uniques [a] deriving (Show, Eq)
instance (Eq a, Arbitrary a) => Arbitrary (Uniques a) where arbitrary = Uniques . nub <$> arbMany 1 10 arbitrary

arbFew :: Gen a -> Gen [a]
arbFew = arbMany 0 4

arbMany :: Int -> Int -> Gen a -> Gen [a]
arbMany min max me = chooseInt (min, max) >>= flip vectorOf me

-- Arbitrary with depth
class Arbitrary a => ArbWithDepth a where
    arbWithDepth :: Int -> Gen a

instance ArbWithDepth a => ArbWithDepth (Maybe a) where
    arbWithDepth depth = oneof
        [ return Nothing
        , Just <$> arbWithDepth depth
        ]

instance ArbWithDepth a => ArbWithDepth [a] where
    arbWithDepth depth = arbFew $ arbWithDepth $ depth-1

instance (Ord k, ArbWithDepth k, ArbWithDepth v) => ArbWithDepth (M.Map k v) where
    arbWithDepth depth =
        let sub :: ArbWithDepth a => Gen a
            sub = arbWithDepth $ depth-1
        in fmap M.fromList $ arbFew $ liftM2 (,) sub sub

arbDepth :: ArbWithDepth a => Gen a
arbDepth = chooseInt (0, 3) >>= arbWithDepth