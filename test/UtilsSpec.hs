module UtilsSpec where

import Test.Hspec
import Test.QuickCheck

import Data.List
import Ops.PzVal
import Ops.PzValSpec
import TestUtils
import Types.PzVal
import Utils

spec :: Spec
spec = do
    unparseSpec
    getDuplicatesSpec
    invalidArityMsgSpec
    f0Spec
    f1Spec
    f2Spec
    f3Spec

unparseSpec :: Spec
unparseSpec = describe "unparseMaybeVal" $ do
    it "unparses to itself using provided func" $ do
        let f Nothing = ""; f (Just v) = unparseVal f v ++ " "
        property $ \(UnparseValid v) -> do
            unparse v `shouldBe` unparseVal f v
            unparse (PzList []) `shouldBe` unparseVal f (PzList [])

getDuplicatesSpec :: Spec
getDuplicatesSpec = describe "getDuplicates" $ do
    it "finds no duplicates in empty list" $ do
        getDuplicates [] `shouldBe` ([] :: [Int])

    it "finds no duplicates" $ do
        property $ \(Uniques xs) -> do
            getDuplicates xs `shouldBe` ([] :: [Int])

    it "finds one duplicate" $ do
        property $ \x (Uniques xs) -> do
            getDuplicates (x:xs ++ [x]) `shouldBe` ([x] :: [Int])
  
    it "finds two duplicates" $ do
        property $ \x y (Uniques xs) -> nub [x, y] == [x, y] ==> do
            getDuplicates (x:y:xs ++ [x, y]) `shouldBe` (sort [x, y] :: [Int])
  
    it "finds three duplicates" $ do
        property $ \x y z (Uniques xs) -> nub [x, y, z] == [x, y, z] ==> do
            getDuplicates (x:y:z:xs ++ [x, y, z]) `shouldBe` (sort [x, y, z] :: [Int])
  
    it "finds N duplicates" $ do
        property $ \(Uniques ds) (Uniques xs) -> do
            getDuplicates (ds ++ xs ++ ds) `shouldBe` (sort ds :: [Int])
  
    it "finds only duplicates" $ do
        property $ \(Uniques xs) -> do
            getDuplicates (xs ++ xs) `shouldBe` (sort xs :: [Int])
  
invalidArityMsgSpec :: Spec
invalidArityMsgSpec = describe "invalidArityMsg" $ do
    it "returns the appropriate message" $ do
        property $ \n (Positive m) -> do
            let xs = replicate m ()
            invalidArityMsg n xs `shouldBe` "Invalid number of arguments. Expected " ++ show n ++ ", got: " ++ show m

f0Spec :: Spec
f0Spec = describe "f0" $ do
    it "succeeds with zero arguments" $ do
        property $ \r -> do
            f0 [] (undefinedOrResult0 r) `shouldBe` r

    it "fails with one or more arguments" $ do
        property $ \v vs -> do
            let xs = v:vs :: [Int]
            f0 xs undef0 `shouldBe` Left (invalidArityMsg 0 xs)

f1Spec :: Spec
f1Spec = describe "f1" $ do
    it "succeeds with one argument" $ do
        property $ \r v -> do
            f1 [v] (undefinedOrResult1 v r) `shouldBe` r

    it "fails with zero, two or more arguments" $ do
        property $ \v1 v2 vs -> do
            let xs = v1:v2:vs
            f1 [] undef1 `shouldBe` Left (invalidArityMsg 1 [])
            f1 xs undef1 `shouldBe` Left (invalidArityMsg 1 xs)

f2Spec :: Spec
f2Spec = describe "f2" $ do
    it "succeeds with two arguments" $ do
        property $ \r v1 v2 -> do
            f2 [v1,v2] (undefinedOrResult2 v1 v2 r) `shouldBe` r

    it "fails with zero, one, three or more arguments" $ do
        property $ \v1 v2 v3 vs -> do
            let xs = v1:v2:v3:vs
            f2 [] undef2 `shouldBe` Left (invalidArityMsg 2 [])
            f2 [v1] undef2 `shouldBe` Left (invalidArityMsg 2 [v1])
            f2 xs undef2 `shouldBe` Left (invalidArityMsg 2 xs)

f3Spec :: Spec
f3Spec = describe "f3" $ do
    it "succeeds with three arguments" $ do
        property $ \r v1 v2 v3 -> do
            f3 [v1,v2,v3] (undefinedOrResult3 v1 v2 v3 r) `shouldBe` r

    it "fails with zero, one, two, four or more arguments" $ do
        property $ \v1 v2 v3 v4 vs -> do
            let xs = v1:v2:v3:v4:vs
            f3 [] undef3 `shouldBe` Left (invalidArityMsg 3 [])
            f3 [v1] undef3 `shouldBe` Left (invalidArityMsg 3 [v1])
            f3 [v1,v2] undef3 `shouldBe` Left (invalidArityMsg 3 [v1,v2])
            f3 xs undef3 `shouldBe` Left (invalidArityMsg 3 xs)

-- Utils
type R = Utils.Result -- conflict with QuickCheck

undefinedOrResult0 :: R Int -> () -> R Int
undefinedOrResult0 r () = r

undefinedOrResult1 :: Int -> R Int -> Int -> R Int
undefinedOrResult1 v r x = if [x] /= [v] then undefined else r

undefinedOrResult2 :: Int -> Int -> R Int -> Int -> Int -> R Int
undefinedOrResult2 v1 v2 r x1 x2 = if [x1, x2] /= [v1, v2] then undefined else r

undefinedOrResult3 :: Int -> Int -> Int -> R Int -> Int -> Int -> Int -> R Int
undefinedOrResult3 v1 v2 v3 r x1 x2 x3 = if [x1, x2, x3] /= [v1, v2, v3] then undefined else r

undef0 :: () -> R Int
undef0 = undefined

undef1 :: Int -> R Int
undef1 = undefined

undef2 :: Int -> Int -> R Int
undef2 = undefined

undef3 :: Int -> Int -> Int -> R Int
undef3 = undefined