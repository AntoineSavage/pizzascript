module Types.SymbSpec where

import Test.Hspec
import Test.QuickCheck

import TestUtils
import Types.Nat
import Types.NatSpec
import Types.Symb

spec :: Spec
spec = describe "Symb" $ do
    it "implements Show" $ do
        property $ \n f ns -> do
            show (Symb Z f ns) `shouldBe` "Symb Z" ++ " " ++ show f ++ " " ++ show ns
            show (Symb (S n) f ns) `shouldBe` "Symb (" ++ show (S n) ++ ") " ++ show f ++ " " ++ show ns

    it "implements Eq" $ do
        property $ \nx ny fx fy nsx nsy  -> do
            Symb nx fx nsx == Symb nx fx nsx `shouldBe` True
            Symb nx fx nsx == Symb ny fx nsx `shouldBe` nx == ny
            Symb nx fx nsx == Symb nx fy nsx `shouldBe` fx == fy
            Symb nx fx nsx == Symb nx fx nsy `shouldBe` nsx == nsy

    it "implements Ord" $ do
        property $ \nx ny fx fy nsx nsy  -> do
            Symb nx fx nsx <= Symb nx fx nsx `shouldBe` True
            Symb nx fx nsx <= Symb ny fx nsx `shouldBe` nx <= ny
            Symb nx fx nsx <= Symb nx fy nsx `shouldBe` fx <= fy
            Symb nx fx nsx <= Symb nx fx nsy `shouldBe` nsx <= nsy

-- Utils
validFirsts = underscore : lettersUpper ++ lettersLower ++ accentChars
validNexts = underscore : digits ++ lettersUpper ++ lettersLower ++ accentChars

instance Arbitrary Symb where
    arbitrary = do
        n <- arbitrary
        Ident f ns <- arbitrary
        return $ Symb n f ns

data Ident = Ident Char String deriving (Show, Eq)
instance Arbitrary Ident where
    arbitrary = do
        first <- elements validFirsts
        nexts <- chooseInt (0, 10) >>= flip vectorOf (elements validNexts)
        return $ Ident first nexts