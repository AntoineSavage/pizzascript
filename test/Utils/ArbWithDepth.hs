module Utils.ArbWithDepth where

import qualified Data.Map as M

import Control.Monad
import TestUtils
import Test.QuickCheck

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