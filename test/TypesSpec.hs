module TypesSpec where
    
import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Text.Parsec.Pos
import Types

spec :: Spec
spec = do
    withPosEqSpec
    withPosOrdSpec

withPosEqSpec :: Spec
withPosEqSpec = describe "WithPos (eq)" $ do
    forM_ [p1, p2] $ \p -> do
        it "ignores position" $ do
            property $ \n m -> do
                let x = AstNum n
                    x' = AstNum $ n + 1
                    y = AstNum m
                (WithPos p1 x) == (WithPos p x) `shouldBe` True
                (WithPos p1 x) == (WithPos p x') `shouldBe` False
                (WithPos p1 x) == (WithPos p y) `shouldBe` x == y

withPosOrdSpec :: Spec
withPosOrdSpec = describe "WithPos (ord)" $ do
    forM_ [p1, p2] $ \p -> do
        it "ignores position" $ do
            property $ \n m -> do
                let x = AstNum n
                    x' = AstNum $ n + 1
                    y = AstNum m
                compare (WithPos p1 x) (WithPos p x) `shouldBe` EQ
                compare (WithPos p1 x) (WithPos p x') `shouldBe` LT
                compare (WithPos p1 x') (WithPos p x) `shouldBe` GT
                compare (WithPos p1 x) (WithPos p y) `shouldBe` compare x y

p1 = newPos "abc" 1 1
p2 = newPos "xyz" 2 2