module TestUtils where

import Data.List
import Data.WithPosSpec
import Test.QuickCheck
import Types
import Utils.ArbWithDepth

newtype Few a = Few [a] deriving (Show, Eq)
instance Arbitrary a => Arbitrary (Few a) where arbitrary = Few <$> arbFew arbitrary

newtype Uniques a = Uniques [a] deriving (Show, Eq)
instance (Eq a, Arbitrary a) => Arbitrary (Uniques a) where arbitrary = Uniques . nub <$> arbMany 1 10 arbitrary