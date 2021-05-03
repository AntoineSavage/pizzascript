module TypesSpec where
    
import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Text.Parsec.Pos
import Types

spec :: Spec
spec = do
    astExprEqSpec
    astExprOrdSpec

astExprEqSpec :: Spec
astExprEqSpec = describe "AstExpr (eq)" $ do
    forM_ [p1, p2] $ \p -> do
        it "ignores position" $ do
            property $ \n -> do
                let v1 = AstNum n
                    v2 = AstNum $ n + 1
                (AstExpr p1 v1) == (AstExpr p v2) `shouldBe` False
                (AstExpr p1 v1) == (AstExpr p v1) `shouldBe` True

astExprOrdSpec :: Spec
astExprOrdSpec = describe "AstExpr (ord)" $ do
    forM_ [p1, p2] $ \p -> do
        it "ignores position" $ do
            property $ \n -> do
                let v1 = AstNum n
                    v2 = AstNum $ n + 1
                compare (AstExpr p1 v1) (AstExpr p v2) `shouldBe` compare v1 v2
                compare (AstExpr p1 v1) (AstExpr p v1) `shouldBe` EQ

p1 = newPos "abc" 1 1
p2 = newPos "xyz" 2 2