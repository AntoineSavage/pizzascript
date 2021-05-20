module TestUtils where

import Data.List
import Test.QuickCheck
import Types

newtype Few a = Few [a] deriving (Show, Eq)
instance Arbitrary a => Arbitrary (Few a) where arbitrary = Few <$> arbFew arbitrary

newtype Uniques a = Uniques [a] deriving (Show, Eq)
instance (Eq a, Arbitrary a) => Arbitrary (Uniques a) where arbitrary = Uniques . nub <$> arbMany 1 10 arbitrary

arbFew :: Gen a -> Gen [a]
arbFew = arbMany 0 4

arbMany :: Int -> Int -> Gen a -> Gen [a]
arbMany min max me = chooseInt (min, max) >>= flip vectorOf me