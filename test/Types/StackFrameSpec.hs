module Types.StackFrameSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Ops.PzVal
import TestUtils
import Types.FuncSpec
import Types.PzVal
import Types.PzValSpec
import Types.StackFrame

spec :: Spec
spec = do
    describe "StackFrame" $ do
        it "implements Show" $ do
            property $ \(ArbDict d) s -> do
                show (StackFrame d s) `shouldBe` "StackFrame (" ++ show d ++ ") (" ++ show s ++ ")"

        it "implements Eq" $ do
            property $ \(ArbDict dx) (ArbDict dy) sx sy -> do
                StackFrame dx sx == StackFrame dx sx `shouldBe` True
                StackFrame dx sx == StackFrame dy sx `shouldBe` dx == dy
                StackFrame dx sx == StackFrame dx sy `shouldBe` sx == sy

    describe "StackFrameSpec" $ do
        it "implements Show" $ do
            property $ \s x (Few xs) (Few ys) (ArbDict d) f -> x /= PzUnit ==> do
                show (Block ys) `shouldBe` "Block " ++ show ys
                
                show (Form Nothing PzUnit ys) `shouldBe` "Form Nothing PzUnit " ++ show ys
                show (Form (Just s) x ys) `shouldBe` "Form (" ++ show (Just s) ++ ") (" ++ show x ++ ") " ++ show ys

                show (Invoc Nothing d f xs Nothing) `shouldBe` "Invoc Nothing (" ++ show d ++ ") (" ++ show f ++ ") " ++ show xs ++ " Nothing"
                show (Invoc (Just s) d f xs (Just ys)) `shouldBe` "Invoc (" ++ show (Just s) ++ ") (" ++ show d ++ ") (" ++ show f ++ ") " ++ show xs ++ " (" ++ show (Just ys) ++ ")"

        it "implements Eq" $ do
            property $ \msx msy x y (Few xs) (Few mxs') (Few ys) (Few mys') (ArbDict dx) (ArbDict dy) fx fy -> do
                Block xs == Block xs `shouldBe` True
                Block xs == Block ys `shouldBe` xs == ys
                Block u == Form u u u `shouldBe` False
                Block u == Invoc u u u u u `shouldBe` False

                Form u u u == Block u `shouldBe` False
                Form msx x xs == Form msx x xs `shouldBe` True
                Form msx x xs == Form msy x xs `shouldBe` msx == msy
                Form msx x xs == Form msx y xs `shouldBe` x == y
                Form msx x xs == Form msx x ys `shouldBe` xs == ys

                let mxs = joinListMaybe mxs'
                    mys = joinListMaybe mys'
                    xs' = map fromQuoted xs
                    ys' = map fromQuoted ys
                Invoc u u u u u == Block u `shouldBe` False
                Invoc u u u u u == Form u u u `shouldBe` False
                Invoc msx dx fx xs' mxs == Invoc msx dx fx xs' mxs `shouldBe` True
                Invoc msx dx fx xs' mxs == Invoc msy dx fx xs' mxs `shouldBe` msx == msy
                Invoc msx dx fx xs' mxs == Invoc msx dy fx xs' mxs `shouldBe` dx == dy
                Invoc msx dx fx xs' mxs == Invoc msx dx fy xs' mxs `shouldBe` fx == fy
                Invoc msx dx fx xs' mxs == Invoc msx dx fx ys' mxs `shouldBe` xs == ys
                Invoc msx dx fx xs' mxs == Invoc msx dx fx xs' mys `shouldBe` mxs == mys

-- Utils
joinListMaybe :: [Maybe a] -> Maybe [a]
joinListMaybe = go [] where
    go acc []     = Just $ reverse acc
    go acc (x:xs) = case x of
        Nothing -> Nothing
        Just y -> go (y:acc) xs

instance Arbitrary StackFrame where arbitrary = arbDepth
instance ArbWithDepth StackFrame where arbWithDepth depth = liftM2 StackFrame arbDepth arbDepth

instance Arbitrary StackFrameSpec where arbitrary = arbDepth
instance ArbWithDepth StackFrameSpec where
    arbWithDepth depth = oneof
        [ fmap Block arbDepth
        , liftM3 Form arbitrary arbDepth arbDepth
        , do
            a <- arbitrary
            b <- arbDepth
            c <- arbDepth
            d <- arbDepth
            e <- arbDepth
            return $ Invoc a b c d e
        ]