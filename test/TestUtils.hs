module TestUtils where

import qualified Data.Map as M

import Control.Monad
import Data.List
import Test.QuickCheck

u = undefined

digits = ['0'..'9']
lettersUpper = ['A'..'Z']
lettersLower = ['a'..'z']
symbols = " !#$%&()*+,-.:;<=>?@[]^`{|}~"
accentChars = "àâäĉèéêëĝĥîïĵôöŝùûüŵŷÿ"
escapees = "\"\\\b\f\n\r\t"
underscore = '_'

leftAsStr :: (Show a, Show b) => Either a b -> String
leftAsStr (Left x)  = show x
leftAsStr x         = "Expected Left, but was: " ++ show x

-- Arbitrary constraints
newtype Few a = Few [a] deriving (Show, Eq)
instance Arbitrary a => Arbitrary (Few a) where arbitrary = Few <$> arbFew arbitrary

newtype Uniques a = Uniques [a] deriving (Show, Eq)
instance (Eq a, Arbitrary a) => Arbitrary (Uniques a) where arbitrary = Uniques . nub <$> arbMany 1 10 arbitrary

data Uniques2 a = Uniques2 a a deriving (Show, Eq)
instance (Eq a, Arbitrary a) => Arbitrary (Uniques2 a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        if nub [x, y] /= [x, y] then arbitrary else return $ Uniques2 x y

data Uniques3 a = Uniques3 a a a deriving (Show, Eq)
instance (Eq a, Arbitrary a) => Arbitrary (Uniques3 a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        if nub [x, y, z] /= [x, y, z] then arbitrary else return $ Uniques3 x y z

arbFew :: Gen a -> Gen [a]
arbFew = arbMany 0 2

arbMany :: Int -> Int -> Gen a -> Gen [a]
arbMany min max me = chooseInt (min, max) >>= flip vectorOf me

-- Arbitrary with depth
class Arbitrary a => ArbWithDepth a where
    arbWithDepth :: Int -> Gen a

instance ArbWithDepth Int where arbWithDepth _ = arbitrary

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
arbDepth = chooseInt (0, 2) >>= arbWithDepth