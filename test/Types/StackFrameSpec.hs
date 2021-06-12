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
            property $ \s x y (Few xs) (Few ys) (ArbDict d) f -> x /= PzUnit ==> do
                show (Block ys) `shouldBe` "Block " ++ show ys
                
                show (FormQuoted Nothing PzUnit ys) `shouldBe` "FormQuoted Nothing PzUnit " ++ show ys
                show (FormQuoted (Just s) y ys) `shouldBe` "FormQuoted (" ++ show (Just s) ++ ") (" ++ show y ++ ") " ++ show ys
                
                show (FormEvaled Nothing PzUnit ys) `shouldBe` "FormEvaled Nothing PzUnit " ++ show ys
                show (FormEvaled (Just s) x ys) `shouldBe` "FormEvaled (" ++ show (Just s) ++ ") (" ++ show x ++ ") " ++ show ys

                show (InvocQuoted Nothing d f ys) `shouldBe` "InvocQuoted Nothing (" ++ show d ++ ") (" ++ show f ++ ") " ++ show ys
                show (InvocQuoted (Just s) d f ys) `shouldBe` "InvocQuoted (" ++ show (Just s) ++ ") (" ++ show d ++ ") (" ++ show f ++ ") " ++ show ys

                show (InvocEvaled Nothing d f xs Nothing) `shouldBe` "InvocEvaled Nothing (" ++ show d ++ ") (" ++ show f ++ ") " ++ show xs ++ " Nothing"
                show (InvocEvaled (Just s) d f xs (Just ys)) `shouldBe` "InvocEvaled (" ++ show (Just s) ++ ") (" ++ show d ++ ") (" ++ show f ++ ") " ++ show xs ++ " (" ++ show (Just ys) ++ ")"

        it "implements Eq" $ do
            property $ \msx msy x y qx qy (Few xs) (Few ys) (Few qxs) (Few qys) (ArbDict dx) (ArbDict dy) fx fy -> do
                Block qxs == Block qxs `shouldBe` True
                Block qxs == Block qys `shouldBe` qxs == qys
                Block u == FormQuoted u u u `shouldBe` False
                Block u == FormEvaled u u u `shouldBe` False
                Block u == InvocQuoted u u u u `shouldBe` False
                Block u == InvocEvaled u u u u u `shouldBe` False

                FormQuoted u u u == Block u `shouldBe` False
                FormQuoted msx qx qxs == FormQuoted msx qx qxs `shouldBe` True
                FormQuoted msx qx qxs == FormQuoted msy qx qxs `shouldBe` msx == msy
                FormQuoted msx qx qxs == FormQuoted msx qy qxs `shouldBe` qx == qy
                FormQuoted msx qx qxs == FormQuoted msx qx qys `shouldBe` qxs == qys
                FormQuoted u u u == FormEvaled u u u `shouldBe` False
                FormQuoted u u u == InvocQuoted u u u u `shouldBe` False
                FormQuoted u u u == InvocEvaled u u u u u `shouldBe` False

                FormEvaled u u u == Block u `shouldBe` False
                FormEvaled u u u == FormQuoted u u u `shouldBe` False
                FormEvaled msx x qxs == FormEvaled msx x qxs `shouldBe` True
                FormEvaled msx x qxs == FormEvaled msy x qxs `shouldBe` msx == msy
                FormEvaled msx x qxs == FormEvaled msx y qxs `shouldBe` x == y
                FormEvaled msx x qxs == FormEvaled msx x qys `shouldBe` qxs == qys
                FormEvaled u u u == InvocQuoted u u u u `shouldBe` False
                FormEvaled u u u == InvocEvaled u u u u u `shouldBe` False

                InvocQuoted u u u u == Block u `shouldBe` False
                InvocQuoted u u u u == FormQuoted u u u `shouldBe` False
                InvocQuoted u u u u == FormEvaled u u u `shouldBe` False
                InvocQuoted msx dx fx qxs == InvocQuoted msx dx fx qxs `shouldBe` True
                InvocQuoted msx dx fx qxs == InvocQuoted msx dy fx qxs `shouldBe` dx == dy
                InvocQuoted msx dx fx qxs == InvocQuoted msx dx fy qxs `shouldBe` fx == fy
                InvocQuoted msx dx fx qxs == InvocQuoted msx dx fx qys `shouldBe` qxs == qys
                InvocQuoted u u u u == InvocEvaled u u u u u `shouldBe` False

                forM_ [Nothing, Just qxs] $ \mqxs -> do
                    InvocEvaled u u u u u == Block u `shouldBe` False
                    InvocEvaled u u u u u == FormQuoted u u u `shouldBe` False
                    InvocEvaled u u u u u == FormEvaled u u u `shouldBe` False
                    InvocEvaled u u u u u == InvocQuoted u u u u `shouldBe` False
                    InvocEvaled msx dx fx xs mqxs == InvocEvaled msx dx fx xs mqxs `shouldBe` True
                    InvocEvaled msx dx fx xs mqxs == InvocEvaled msy dx fx xs mqxs `shouldBe` msx == msy
                    InvocEvaled msx dx fx xs mqxs == InvocEvaled msx dy fx xs mqxs `shouldBe` dx == dy
                    InvocEvaled msx dx fx xs mqxs == InvocEvaled msx dx fy xs mqxs `shouldBe` fx == fy
                    InvocEvaled msx dx fx xs mqxs == InvocEvaled msx dx fx ys mqxs `shouldBe` xs == ys
                    InvocEvaled msx dx fx xs mqxs == InvocEvaled msx dx fx xs (Just qys) `shouldBe` mqxs == Just qys

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
            , liftM3 FormQuoted arbitrary sub sub
            , liftM3 FormEvaled arbitrary sub sub
            , liftM4 InvocQuoted arbitrary sub sub sub
            , liftM5 InvocEvaled arbitrary sub sub sub sub
            ]