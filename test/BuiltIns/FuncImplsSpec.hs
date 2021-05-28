module BuiltIns.FuncImplsSpec where

import Test.Hspec
import Test.QuickCheck

import BuiltIns.FuncImpls
import BuiltIns.FuncValues
import Control.Monad
import Ops.BoolishSpec
import Ops.PzValSpec
import Symbs
import TestUtils
import Types.Numb
import Types.PzVal
import Types.PzValSpec
import Types.Str

spec :: Spec
spec = do
    _typeOfSpec
    _eqSpec
    _ltSpec
    ifThenElseSpec
    _notSpec
    _orSpec
    _andSpec

_typeOfSpec :: Spec
_typeOfSpec = describe "_typeOf" $ do
    it "returns the type symbol without evaluating too much" $ do
        _typeOf PzUnit `shouldBe` PzUnit
        _typeOf (PzNum u) `shouldBe` pzSymbNum
        _typeOf (PzStr u) `shouldBe` pzSymbStr
        _typeOf (PzSymb u) `shouldBe` pzSymbSymb
        _typeOf (PzList u) `shouldBe` pzSymbList
        _typeOf (PzDict u) `shouldBe` pzSymbDict
        _typeOf (PzFunc u u) `shouldBe` pzSymbFunc

    it "never fails for any value" $ do
        property $ \v -> do
            show (_typeOf v) `shouldNotBe` ""

_eqSpec :: Spec
_eqSpec = describe "_eq" $ do
    -- TODO Check all type combinations (esp. func)
    it "returns false for different values" $ do
        property $ \v -> do
            _eq v (PzList [v]) `shouldBe` pzSymbFalse

    it "returns true for equal values" $ do
        property $ \v -> do
            _eq v v `shouldBe` pzSymbTrue

_ltSpec :: Spec
_ltSpec = describe "_lt" $ do
    context "given the unit type"  $ do
        it "compares against itself (false)" $ do
            _lt PzUnit PzUnit `shouldBe` pzSymbFalse

        it "compares against greater types (true)" $ do
            forM_   [ PzNum u
                    , PzStr u
                    , PzSymb u
                    , PzList u
                    , PzDict u
                    , PzFunc u u
                    ] $ \y -> do
                _lt PzUnit y `shouldBe` pzSymbTrue

    context "given numbers"  $ do
        it "compares against lesser types (false)" $ do
            forM_   [ PzUnit
                    ] $ \y -> do
                _lt (PzNum u) y `shouldBe` pzSymbFalse

        it "compares against itself (false)" $ do
            property $ \x -> do
                _lt (PzNum x) (PzNum x) `shouldBe` pzSymbFalse

        it "compares against other numbers" $ do
            property $ \x y -> x /= y ==> do
                let left = min x y
                    right = max x y
                _lt (PzNum left) (PzNum right) `shouldBe` pzSymbTrue
                _lt (PzNum right) (PzNum left) `shouldBe` pzSymbFalse

        it "compares against greater types (true)" $ do
            forM_   [ PzStr u
                    , PzSymb u
                    , PzList u
                    , PzDict u
                    , PzFunc u u
                    ] $ \y -> do
                _lt (PzNum u) y `shouldBe` pzSymbTrue

    context "given strings"  $ do
        it "compares against lesser types (false)" $ do
            forM_   [ PzUnit
                    , PzNum u           
                    ] $ \y -> do
                _lt (PzStr u) y `shouldBe` pzSymbFalse

        it "compares against itself (false)" $ do
            property $ \x -> do
                _lt (PzStr x) (PzStr x) `shouldBe` pzSymbFalse

        it "compares against other strings" $ do
            property $ \x y -> x /= y ==> do
                let left = min x y
                    right = max x y
                _lt (PzStr left) (PzStr right) `shouldBe` pzSymbTrue
                _lt (PzStr right) (PzStr left) `shouldBe` pzSymbFalse

        it "compares against greater types (true)" $ do
            forM_   [ PzSymb u
                    , PzList u
                    , PzDict u
                    , PzFunc u u
                    ] $ \y -> do
                _lt (PzStr u) y `shouldBe` pzSymbTrue

    context "given symbols"  $ do
        it "compares against lesser types (false)" $ do
            forM_   [ PzUnit
                    , PzNum u     
                    , PzStr u     
                    ] $ \y -> do
                _lt (PzSymb u) y `shouldBe` pzSymbFalse

        it "compares against itself (false)" $ do
            property $ \x -> do
                _lt (PzSymb x) (PzSymb x) `shouldBe` pzSymbFalse

        it "compares against other symbols" $ do
            property $ \x y -> x /= y ==> do
                let left = min x y
                    right = max x y
                _lt (PzSymb left) (PzSymb right) `shouldBe` pzSymbTrue
                _lt (PzSymb right) (PzSymb left) `shouldBe` pzSymbFalse

        it "compares against greater types (true)" $ do
            forM_   [ PzList u
                    , PzDict u
                    , PzFunc u u
                    ] $ \y -> do
                _lt (PzSymb u) y `shouldBe` pzSymbTrue

    context "given lists"  $ do
        it "compares against lesser types (false)" $ do
            forM_   [ PzUnit
                    , PzNum u     
                    , PzStr u     
                    , PzSymb u
                    ] $ \y -> do
                _lt (PzList u) y `shouldBe` pzSymbFalse

        it "compares against itself (false)" $ do
            property $ \(Few x) -> do
                _lt (PzList x) (PzList x) `shouldBe` pzSymbFalse

        it "compares against other lists" $ do
            property $ \(Few x) y -> x /= y ==> do
                let left = min x y
                    right = max x y
                _lt (PzList left) (PzList right) `shouldBe` pzSymbTrue
                _lt (PzList right) (PzList left) `shouldBe` pzSymbFalse

        it "compares against greater types (true)" $ do
            forM_   [ PzDict u
                    , PzFunc u u
                    ] $ \y -> do
                _lt (PzList u) y `shouldBe` pzSymbTrue

    context "given dictionaries"  $ do
        it "compares against lesser types (false)" $ do
            forM_   [ PzUnit
                    , PzNum u     
                    , PzStr u     
                    , PzSymb u
                    , PzList u
                    ] $ \y -> do
                _lt (PzDict u) y `shouldBe` pzSymbFalse

        it "compares against itself (false)" $ do
            property $ \(ArbDict x) -> do
                _lt (PzDict x) (PzDict x) `shouldBe` pzSymbFalse

        it "compares against other dictionaries" $ do
            property $ \(ArbDict x) (ArbDict y) -> x /= y ==> do
                let left = min x y
                    right = max x y
                _lt (PzDict left) (PzDict right) `shouldBe` pzSymbTrue
                _lt (PzDict right) (PzDict left) `shouldBe` pzSymbFalse

        it "compares against greater types (true)" $ do
            forM_   [ PzFunc u u
                    ] $ \y -> do
                _lt (PzDict u) y `shouldBe` pzSymbTrue

    context "given functions"  $ do
        it "compares against lesser types (false)" $ do
            forM_   [ PzUnit
                    , PzNum u     
                    , PzStr u     
                    , PzSymb u
                    , PzList u
                    , PzDict u
                    ] $ \y -> do
                _lt (PzFunc u u) y `shouldBe` pzSymbFalse

        it "compares against itself (false)" $ do
            property $ \x -> do
                _lt (PzFunc u x) (PzFunc u x) `shouldBe` pzSymbFalse

        it "compares against other functions" $ do
            property $ \x y -> x /= y ==> do
                let left = min x y
                    right = max x y
                _lt (PzFunc u left) (PzFunc u right) `shouldBe` pzSymbTrue
                _lt (PzFunc u right) (PzFunc u left) `shouldBe` pzSymbFalse

ifThenElseSpec :: Spec
ifThenElseSpec = describe "simulate if-then-else with not-or-and" $ do
    let ifThenElse p_ t f = let p = _not $ _not p_ in _or (_and p t) (_and (_not p) f)
    it "returns x when p is true" $ do
        property $ \x -> do
            ifThenElse pzSymbTrue x u `shouldBe` x

    it "returns x when p is truish" $ do
        property $ \x (PzTruish p) -> do
            ifThenElse p x u `shouldBe` x

    it "returns y when p is falsish" $ do
        property $ \y (PzFalsish p) -> do
            ifThenElse p u y `shouldBe` y

    it "returns y when p is false" $ do
        property $ \y -> do
            ifThenElse pzSymbFalse u y `shouldBe` y

_notSpec :: Spec
_notSpec = describe "_not" $ do
    it "returns true for false and vice-versa" $ do
        _not pzSymbFalse `shouldBe` pzSymbTrue
        _not pzSymbTrue `shouldBe` pzSymbFalse

    it "returns true for falsish and vice-versa" $ do
        property $ \(PzFalsish x) (PzTruish y) -> do
            _not x `shouldBe` pzSymbTrue
            _not y `shouldBe` pzSymbFalse

_orSpec :: Spec
_orSpec = describe "_or (truest wins)" $ do
    it "returns x for x=true, y=*" $ do
        property $ \y -> do
            let x = pzSymbTrue
            _or x y `shouldBe` x

    it "returns y for x=truish, y=true" $ do
        property $ \(PzTruish x) -> do
            let y = pzSymbTrue
            _or x y `shouldBe` y

    it "returns x for x=truish, y=truish|falsish|false" $ do
        property $ \(PzTruish x) (PzTruish y) (PzFalsish y') -> do
            let y'' = pzSymbFalse
            _or x y `shouldBe` x
            _or x y' `shouldBe` x
            _or x y'' `shouldBe` x

    it "returns x for x=falsish, y=false" $ do
        property $ \(PzFalsish x) -> do
            let y = pzSymbFalse
            _or x y `shouldBe` x

    it "returns y for x=falsish, y=true|truish|falsish" $ do
        property $ \(PzFalsish x) (PzTruish y') (PzFalsish y'') -> do
            let y = pzSymbTrue
            _or x y `shouldBe` y
            _or x y'' `shouldBe` y''

    it "returns y for x=false, y=*" $ do
        property $ \(PzFalsish x) y -> do
            _or x y `shouldBe` y

_andSpec :: Spec
_andSpec = describe "_and (falsest wins)" $ do
    it "returns x for x=false, y=*" $ do
        property $ \y -> do
            let x = pzSymbFalse
            _and x y `shouldBe` x

    it "returns y for x=falsish, y=false" $ do
        property $ \(PzFalsish x) -> do
            let y = pzSymbFalse
            _and x y `shouldBe` y

    it "returns x for x=falsish, y=falsish|truish|true" $ do
        property $ \(PzFalsish x) (PzFalsish y) (PzTruish y') -> do
            let y'' = pzSymbTrue
            _and x y `shouldBe` x
            _and x y' `shouldBe` x
            _and x y'' `shouldBe` x

    it "returns x for x=truish, y=true" $ do
        property $ \(PzTruish x) -> do
            let y = pzSymbTrue
            _and x y `shouldBe` x

    it "returns y for x=truish, y=false|falsish|truish" $ do
        property $ \(PzTruish x) (PzFalsish y') (PzTruish y'') -> do
            let y = pzSymbFalse
            _and x y `shouldBe` y
            _and x y'' `shouldBe` y''

    it "returns y for x=true, y=*" $ do
        property $ \(PzTruish x) y -> do
            _and x y `shouldBe` y