module Types.StackFrameSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

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
            property $ \x y (Few xs) (Few ys) (ArbDict d) f -> x /= PzUnit ==> do
                show (Block ys) `shouldBe` "Block " ++ show ys
                
                show (FormQuoted PzUnit ys) `shouldBe` "FormQuoted PzUnit " ++ show ys
                show (FormQuoted y ys) `shouldBe` "FormQuoted (" ++ show y ++ ") " ++ show ys
                
                show (FormEvaled PzUnit ys) `shouldBe` "FormEvaled PzUnit " ++ show ys
                show (FormEvaled x ys) `shouldBe` "FormEvaled (" ++ show x ++ ") " ++ show ys

                show (InvocQuoted d f ys) `shouldBe` "InvocQuoted (" ++ show d ++ ") (" ++ show f ++ ") " ++ show ys

                show (InvocEvaled d f xs Nothing) `shouldBe` "InvocEvaled (" ++ show d ++ ") (" ++ show f ++ ") " ++ show xs ++ " Nothing"
                show (InvocEvaled d f xs (Just ys)) `shouldBe` "InvocEvaled (" ++ show d ++ ") (" ++ show f ++ ") " ++ show xs ++ " (" ++ show (Just ys) ++ ")"

        it "implements Eq" $ do
            property $ \x y qx qy (Few xs) (Few ys) (Few qxs) (Few qys) (ArbDict dx) (ArbDict dy) fx fy -> do
                Block qxs == Block qxs `shouldBe` True
                Block qxs == Block qys `shouldBe` qxs == qys
                Block u == FormQuoted u u `shouldBe` False
                Block u == FormEvaled u u `shouldBe` False
                Block u == InvocQuoted u u u `shouldBe` False
                Block u == InvocEvaled u u u u `shouldBe` False

                FormQuoted u u == Block u `shouldBe` False
                FormQuoted qx qxs == FormQuoted qx qxs `shouldBe` True
                FormQuoted qx qxs == FormQuoted qy qxs `shouldBe` qx == qy
                FormQuoted qx qxs == FormQuoted qx qys `shouldBe` qxs == qys
                FormQuoted u u == FormEvaled u u `shouldBe` False
                FormQuoted u u == InvocQuoted u u u `shouldBe` False
                FormQuoted u u == InvocEvaled u u u u `shouldBe` False

                FormEvaled u u == Block u `shouldBe` False
                FormEvaled u u == FormQuoted u u `shouldBe` False
                FormEvaled x qxs == FormEvaled x qxs `shouldBe` True
                FormEvaled x qxs == FormEvaled y qxs `shouldBe` x == y
                FormEvaled x qxs == FormEvaled x qys `shouldBe` qxs == qys
                FormEvaled u u == InvocQuoted u u u `shouldBe` False
                FormEvaled u u == InvocEvaled u u u u `shouldBe` False

                InvocQuoted u u u == Block u `shouldBe` False
                InvocQuoted u u u == FormQuoted u u `shouldBe` False
                InvocQuoted u u u == FormEvaled u u `shouldBe` False
                InvocQuoted dx fx qxs == InvocQuoted dx fx qxs `shouldBe` True
                InvocQuoted dx fx qxs == InvocQuoted dy fx qxs `shouldBe` dx == dy
                InvocQuoted dx fx qxs == InvocQuoted dx fy qxs `shouldBe` fx == fy
                InvocQuoted dx fx qxs == InvocQuoted dx fx qys `shouldBe` qxs == qys
                InvocQuoted u u u == InvocEvaled u u u u `shouldBe` False

                forM_ [Nothing, Just qxs] $ \mqxs -> do
                    InvocEvaled u u u u == Block u `shouldBe` False
                    InvocEvaled u u u u == FormQuoted u u `shouldBe` False
                    InvocEvaled u u u u == FormEvaled u u `shouldBe` False
                    InvocEvaled u u u u == InvocQuoted u u u `shouldBe` False
                    InvocEvaled dx fx xs mqxs == InvocEvaled dx fx xs mqxs `shouldBe` True
                    InvocEvaled dx fx xs mqxs == InvocEvaled dy fx xs mqxs `shouldBe` dx == dy
                    InvocEvaled dx fx xs mqxs == InvocEvaled dx fy xs mqxs `shouldBe` fx == fy
                    InvocEvaled dx fx xs mqxs == InvocEvaled dx fx ys mqxs `shouldBe` xs == ys
                    InvocEvaled dx fx xs mqxs == InvocEvaled dx fx xs (Just qys) `shouldBe` mqxs == Just qys

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
    arbWithDepth depth =
        let sub :: ArbWithDepth a => Gen a
            sub = arbWithDepth depth
        in oneof
            [ fmap Block sub
            , liftM2 FormQuoted sub sub
            , liftM2 FormEvaled sub sub
            , liftM3 InvocQuoted sub sub sub
            , liftM4 InvocEvaled sub sub sub sub
            ]