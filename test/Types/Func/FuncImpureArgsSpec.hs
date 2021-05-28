module Types.Func.FuncImpureArgsSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Ops.Func.ArgPassSpec
import Ops.SymbSpec
import TestUtils
import Types.Func.FuncImpureArgs

spec :: Spec
spec = describe "FuncImpureArgs" $ do
    it "implements Show" $ do
        property $ \ap ec -> do
            show None `shouldBe` "None"
            show (ArgPass ap) `shouldBe` "ArgPass " ++ show ap
            show (Both ap ec) `shouldBe` "Both " ++ show ap ++ " (" ++ show ec ++ ")"

    it "implements Eq" $ do
        property $ \apx apy ecx ecy -> apx /= apy && ecx /= ecy ==> do
            None == None `shouldBe` True
            None == ArgPass undefined `shouldBe` False
            None == Both undefined undefined `shouldBe` False
            
            ArgPass undefined == None `shouldBe` False
            ArgPass apx == ArgPass apx `shouldBe` True
            ArgPass apx == ArgPass apy `shouldBe` False
            ArgPass undefined == Both undefined undefined `shouldBe` False
            
            Both undefined undefined == None `shouldBe` False
            Both undefined undefined == ArgPass undefined `shouldBe` False
            Both apx ecx == Both apx ecx `shouldBe` True
            Both apx ecx == Both apy ecx `shouldBe` False
            Both apx ecx == Both apx ecy `shouldBe` False

    it "implements Ord" $ do
        property $ \apx apy ecx ecy -> apx /= apy && ecx /= ecy ==> do
            None <= None `shouldBe` True
            None <= ArgPass undefined `shouldBe` True
            None <= Both undefined undefined `shouldBe` True
            
            ArgPass undefined <= None `shouldBe` False
            ArgPass apx <= ArgPass apx `shouldBe` True
            ArgPass apx <= ArgPass apy `shouldBe` apx <= apy
            ArgPass undefined <= Both undefined undefined `shouldBe` True
            
            Both undefined undefined <= None `shouldBe` False
            Both undefined undefined <= ArgPass undefined `shouldBe` False
            Both apx ecx <= Both apx ecx `shouldBe` True
            Both apx ecx <= Both apy ecx `shouldBe` apx <= apy
            Both apx ecx <= Both apx ecy `shouldBe` ecx <= ecy

-- Utils
instance Arbitrary FuncImpureArgs where
    arbitrary = oneof
        [ return None
        , ArgPass <$> arbitrary
        , liftM2 Both arbitrary arbQuotedIdent
        ]