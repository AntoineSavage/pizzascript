{-# LANGUAGE FlexibleInstances #-}
module Types.PzValSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Symbs
import TestUtils
import Types.FuncSpec
import Types.NumbSpec
import Types.PzVal
import Types.StrSpec
import Types.SymbSpec

spec :: Spec
spec = do
    describe "DictKey" $ do
        it "ignores PzFunc implicit context in Show" $ do
            property $ \x f -> not (isFunc x) ==> do
                show (DictKey $ PzFunc undefined f) `shouldBe` "PzFunc <implCtx> (" ++ show f ++ ")"
                show (DictKey x) `shouldBe` show x

        it "ignores PzFunc implicit context in Eq" $ do
            property $ \x y fx fy -> not (isFunc x || isFunc y) ==> do
                DictKey (PzFunc undefined fx) == DictKey (PzFunc undefined fx) `shouldBe` True
                DictKey (PzFunc undefined fx) == DictKey (PzFunc undefined fy) `shouldBe` fx == fy

                DictKey x == DictKey x `shouldBe` True
                DictKey x == DictKey y `shouldBe` x == y

        it "ignores PzFunc implicit context in Ord" $ do
            property $ \x y fx fy -> not (isFunc x || isFunc y) ==> do
                DictKey (PzFunc undefined fx) <= DictKey (PzFunc undefined fx) `shouldBe` True
                DictKey (PzFunc undefined fx) <= DictKey (PzFunc undefined fy) `shouldBe` fx <= fy

                DictKey x <= DictKey x `shouldBe` True
                DictKey x <= DictKey y `shouldBe` x <= y

    describe "PzVal" $ do
        it "implements Show" $ do
            property $ \n s sym (Few xs) (ArbDict d) f -> do
                let _ = xs :: [PzVal Evaled]
                show PzUnit `shouldBe` "PzUnit"
                show (PzNum n) `shouldBe` "PzNum (" ++ show n ++ ")"
                show (PzStr s) `shouldBe` "PzStr (" ++ show s ++ ")"
                show (PzSymb sym) `shouldBe` "PzSymb (" ++ show sym ++ ")"
                show (PzList xs) `shouldBe` "PzList " ++ show xs
                show (PzDict d) `shouldBe` "PzDict (" ++ show d ++ ")"
                show (PzFunc d f) `shouldBe` "PzFunc (" ++ show d ++ ") (" ++ show f ++ ")"

        it "implements Eq" $ do
            property $ \nx ny sx sy symx symy (Few xs) (Few ys) (ArbDict dx) (ArbDict dy) fx fy -> do
                let _ = xs :: [PzVal Evaled]
                PzUnit == PzUnit `shouldBe` True
                PzUnit == PzNum u `shouldBe` False
                PzUnit == PzStr u `shouldBe` False
                PzUnit == PzSymb u `shouldBe` False
                PzUnit == PzList u `shouldBe` False
                PzUnit == PzDict u `shouldBe` False
                PzUnit == PzFunc u u `shouldBe` False

                PzNum u == PzUnit `shouldBe` False
                PzNum nx == PzNum nx `shouldBe` True
                PzNum nx == PzNum ny `shouldBe` nx == ny
                PzNum u == PzStr u `shouldBe` False
                PzNum u == PzSymb u `shouldBe` False
                PzNum u == PzList u `shouldBe` False
                PzNum u == PzDict u `shouldBe` False
                PzNum u == PzFunc u u `shouldBe` False

                PzStr u == PzUnit `shouldBe` False
                PzStr u == PzNum u `shouldBe` False
                PzStr sx == PzStr sx `shouldBe` True
                PzStr sx == PzStr sy `shouldBe` sx == sy
                PzStr u == PzSymb u `shouldBe` False
                PzStr u == PzList u `shouldBe` False
                PzStr u == PzDict u `shouldBe` False
                PzStr u == PzFunc u u `shouldBe` False

                PzSymb u == PzUnit `shouldBe` False
                PzSymb u == PzNum u `shouldBe` False
                PzSymb u == PzStr u `shouldBe` False
                PzSymb symx == PzSymb symx `shouldBe` True
                PzSymb symx == PzSymb symy `shouldBe` symx == symy
                PzSymb u == PzList u `shouldBe` False
                PzSymb u == PzDict u `shouldBe` False
                PzSymb u == PzFunc u u `shouldBe` False

                PzList u == PzUnit `shouldBe` False
                PzList u == PzNum u `shouldBe` False
                PzList u == PzStr u `shouldBe` False
                PzList u == PzSymb u `shouldBe` False
                PzList xs == PzList xs `shouldBe` True
                PzList xs == PzList ys `shouldBe` xs == ys
                PzList u == PzDict u `shouldBe` False
                PzList u == PzFunc u u `shouldBe` False

                PzDict u == PzUnit `shouldBe` False
                PzDict u == PzNum u `shouldBe` False
                PzDict u == PzStr u `shouldBe` False
                PzDict u == PzSymb u `shouldBe` False
                PzDict u == PzList u `shouldBe` False
                PzDict dx == PzDict dx `shouldBe` True
                PzDict dx == PzDict dy `shouldBe` dx == dy
                PzDict u == PzFunc u u `shouldBe` False

                PzFunc u u == PzUnit `shouldBe` False
                PzFunc u u == PzNum u `shouldBe` False
                PzFunc u u == PzStr u `shouldBe` False
                PzFunc u u == PzSymb u `shouldBe` False
                PzFunc u u == PzList u `shouldBe` False
                PzFunc u u == PzDict u `shouldBe` False
                PzFunc dx fx == PzFunc dx fx `shouldBe` True
                PzFunc dx fx == PzFunc dy fx `shouldBe` dx == dy
                PzFunc dx fx == PzFunc dx fy `shouldBe` fx == fy

        it "implements Ord" $ do
            property $ \nx ny sx sy symx symy (Few xs) (Few ys) (ArbDict dx) (ArbDict dy) fx fy -> do
                let _ = xs :: [PzVal Evaled]
                PzUnit <= PzUnit `shouldBe` True
                PzUnit <= PzNum u `shouldBe` True
                PzUnit <= PzStr u `shouldBe` True
                PzUnit <= PzSymb u `shouldBe` True
                PzUnit <= PzList u `shouldBe` True
                PzUnit <= PzDict u `shouldBe` True
                PzUnit <= PzFunc u u `shouldBe` True

                PzNum u <= PzUnit `shouldBe` False
                PzNum nx <= PzNum nx `shouldBe` True
                PzNum nx <= PzNum ny `shouldBe` nx <= ny
                PzNum u <= PzStr u `shouldBe` True
                PzNum u <= PzSymb u `shouldBe` True
                PzNum u <= PzList u `shouldBe` True
                PzNum u <= PzDict u `shouldBe` True
                PzNum u <= PzFunc u u `shouldBe` True

                PzStr u <= PzUnit `shouldBe` False
                PzStr u <= PzNum u `shouldBe` False
                PzStr sx <= PzStr sx `shouldBe` True
                PzStr sx <= PzStr sy `shouldBe` sx <= sy
                PzStr u <= PzSymb u `shouldBe` True
                PzStr u <= PzList u `shouldBe` True
                PzStr u <= PzDict u `shouldBe` True
                PzStr u <= PzFunc u u `shouldBe` True

                PzSymb u <= PzUnit `shouldBe` False
                PzSymb u <= PzNum u `shouldBe` False
                PzSymb u <= PzStr u `shouldBe` False
                PzSymb symx <= PzSymb symx `shouldBe` True
                PzSymb symx <= PzSymb symy `shouldBe` symx <= symy
                PzSymb u <= PzList u `shouldBe` True
                PzSymb u <= PzDict u `shouldBe` True
                PzSymb u <= PzFunc u u `shouldBe` True

                PzList u <= PzUnit `shouldBe` False
                PzList u <= PzNum u `shouldBe` False
                PzList u <= PzStr u `shouldBe` False
                PzList u <= PzSymb u `shouldBe` False
                PzList xs <= PzList xs `shouldBe` True
                PzList xs <= PzList ys `shouldBe` xs <= ys
                PzList u <= PzDict u `shouldBe` True
                PzList u <= PzFunc u u `shouldBe` True

                PzDict u <= PzUnit `shouldBe` False
                PzDict u <= PzNum u `shouldBe` False
                PzDict u <= PzStr u `shouldBe` False
                PzDict u <= PzSymb u `shouldBe` False
                PzDict u <= PzList u `shouldBe` False
                PzDict dx <= PzDict dx `shouldBe` True
                PzDict dx <= PzDict dy `shouldBe` dx <= dy
                PzDict u <= PzFunc u u `shouldBe` True

                PzFunc u u <= PzUnit `shouldBe` False
                PzFunc u u <= PzNum u `shouldBe` False
                PzFunc u u <= PzStr u `shouldBe` False
                PzFunc u u <= PzSymb u `shouldBe` False
                PzFunc u u <= PzList u `shouldBe` False
                PzFunc u u <= PzDict u `shouldBe` False
                PzFunc dx fx <= PzFunc dx fx `shouldBe` True
                PzFunc dx fx <= PzFunc dy fx `shouldBe` dx <= dy
                PzFunc dx fx <= PzFunc dx fy `shouldBe` fx <= fy

-- Utils
isFunc :: PzVal Evaled -> Bool
isFunc (PzFunc _ _) = True
isFunc _            = False

instance Arbitrary (PzVal Evaled) where arbitrary = arbDepth
instance ArbWithDepth (PzVal Evaled) where
    arbWithDepth depth = oneof $
        [ return PzUnit
        , PzNum <$> arbitrary
        , PzStr <$> arbitrary
        , PzSymb <$> arbitrary
        ] ++
        (if depth <= 0 then [] else
            [ fmap PzList $ arbWithDepth depth
            , fmap PzDict $ arbWithDepth depth
            , liftM2 PzFunc (arbWithDepth depth) $ arbWithDepth depth
            ]
        )

newtype ArbDict = ArbDict Dict deriving (Show, Eq)
instance Arbitrary ArbDict where arbitrary = arbDepth
instance ArbWithDepth ArbDict where arbWithDepth depth = ArbDict <$> arbWithDepth depth

instance Arbitrary DictKey where arbitrary = arbDepth
instance ArbWithDepth DictKey where arbWithDepth depth = DictKey <$> arbWithDepth depth

instance Arbitrary (PzVal Quoted) where arbitrary = arbDepth
instance ArbWithDepth (PzVal Quoted) where
    arbWithDepth depth = oneof $
        [ return $ PzList []
        , PzNum <$> arbitrary
        , PzStr <$> arbitrary
        , PzSymb <$> arbitrary
        ] ++
        ( if depth <= 0 then [] else
            [ PzList . (PzSymb symbList:) <$> arbWithDepth depth
            , PzList . (PzSymb symbDict:) <$> arbWithDepth depth
            , PzList <$> arbWithDepth depth
            ]
        )