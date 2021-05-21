module BuiltInsSpec where

import qualified Data.Map as M

import Test.Hspec
import Test.QuickCheck

import BuiltIns
import Data.Boolish
import Data.BoolishSpec
import Data.Func
import Data.Func.ArgPass
import Data.Func.FuncArgs
import Data.Func.FuncBody
import Data.Func.FuncImpureArgs
import Data.Numb
import Data.PzVal
import Data.Str
import Data.Symb
import Data.WithPos
import Idents
import Impls
import Symbs
import Utils
import Values

spec :: Spec
spec = do
    builtInCtxSpec
    constantsSpec
    funcSpec
    ifThenElseSpec
    _notSpec
    _orSpec
    _andSpec

builtInCtxSpec :: Spec
builtInCtxSpec = describe "builtInCtx" $ do
    it "contains the required keys" $ do
        let keyIdents = [identFalse, identTrue, identNot, identOr, identAnd, identFunc]
            keys = flip map keyIdents $ withPos . PzSymb . symb
            values = [pzFalse, pzTrue, pzNot, pzOr, pzAnd, pzFunc]
        builtInCtx `shouldBe` M.fromList (zip keys values)

constantsSpec :: Spec
constantsSpec = describe "constants" $ do
    it "declares boolean constants" $ do
        pzFalse `shouldBe` withPos (PzSymb symbFalse)
        pzTrue `shouldBe` withPos (PzSymb symbTrue)
        pzNot `shouldBe` withPos (PzFunc M.empty $ Func None (ArgsArity builtInPos [withPos identX]) (BodyBuiltIn identNot))
        pzOr `shouldBe` withPos (PzFunc M.empty $ Func None (ArgsArity builtInPos [withPos identX, withPos identY]) (BodyBuiltIn identOr))
        pzAnd `shouldBe` withPos (PzFunc M.empty $ Func None (ArgsArity builtInPos [withPos identX, withPos identY]) (BodyBuiltIn identAnd))

    it "declares function constants" $ do
        pzFunc `shouldBe` withPos (PzFunc M.empty func)

funcSpec :: Spec
funcSpec = describe "func" $ do
    it "returns func value" $ do
        let f = func
        impArgs f `shouldBe` Both builtInPos (withPos Quote) (withPos identCtx)
        args f `shouldBe` ArgsVaria (withPos identArgs)
        body f `shouldBe` BodyBuiltIn identFunc

ifThenElseSpec :: Spec
ifThenElseSpec = describe "simulate if-then-else with not-or-and" $ do
    let ifThenElse p_ t f = let p = _not $ _not p_ in _or (_and p t) (_and (_not p) f)
    it "returns x when p is true" $ do
        property $ \x -> do
            ifThenElse pzTrue x undefined `shouldBe` x

    it "returns x when p is truish" $ do
        property $ \x (PzTruish p) -> do
            ifThenElse p x undefined `shouldBe` x

    it "returns y when p is falsish" $ do
        property $ \y (PzFalsish p) -> do
            ifThenElse p undefined y `shouldBe` y

    it "returns y when p is false" $ do
        property $ \y -> do
            ifThenElse pzFalse undefined y `shouldBe` y

_notSpec :: Spec
_notSpec = describe "_not" $ do
    it "returns true for false and vice-versa" $ do
        _not pzFalse `shouldBe` pzTrue
        _not pzTrue `shouldBe` pzFalse

    it "returns true for falsish and vice-versa" $ do
        property $ \(PzFalsish x) (PzTruish y) -> do
            _not x `shouldBe` pzTrue
            _not y `shouldBe` pzFalse

_orSpec :: Spec
_orSpec = describe "_or (truest wins)" $ do
    it "returns x for x=true, y=*" $ do
        property $ \y -> do
            let x = pzTrue
            _or x y `shouldBe` x

    it "returns y for x=truish, y=true" $ do
        property $ \(PzTruish x) -> do
            let y = pzTrue
            _or x y `shouldBe` y

    it "returns x for x=truish, y=truish|falsish|false" $ do
        property $ \(PzTruish x) (PzTruish y) (PzFalsish y') -> do
            let y'' = pzFalse
            _or x y `shouldBe` x
            _or x y' `shouldBe` x
            _or x y'' `shouldBe` x

    it "returns x for x=falsish, y=false" $ do
        property $ \(PzFalsish x) -> do
            let y = pzFalse
            _or x y `shouldBe` x

    it "returns y for x=falsish, y=true|truish|falsish" $ do
        property $ \(PzFalsish x) (PzTruish y') (PzFalsish y'') -> do
            let y = pzTrue
            _or x y `shouldBe` y
            _or x y'' `shouldBe` y''

    it "returns y for x=false, y=*" $ do
        property $ \(PzFalsish x) y -> do
            _or x y `shouldBe` y

_andSpec :: Spec
_andSpec = describe "_and (falsest wins)" $ do
    it "returns x for x=false, y=*" $ do
        property $ \y -> do
            let x = pzFalse
            _and x y `shouldBe` x

    it "returns y for x=falsish, y=false" $ do
        property $ \(PzFalsish x) -> do
            let y = pzFalse
            _and x y `shouldBe` y

    it "returns x for x=falsish, y=falsish|truish|true" $ do
        property $ \(PzFalsish x) (PzFalsish y) (PzTruish y') -> do
            let y'' = pzTrue
            _and x y `shouldBe` x
            _and x y' `shouldBe` x
            _and x y'' `shouldBe` x

    it "returns x for x=truish, y=true" $ do
        property $ \(PzTruish x) -> do
            let y = pzTrue
            _and x y `shouldBe` x

    it "returns y for x=truish, y=false|falsish|truish" $ do
        property $ \(PzTruish x) (PzFalsish y') (PzTruish y'') -> do
            let y = pzFalse
            _and x y `shouldBe` y
            _and x y'' `shouldBe` y''

    it "returns y for x=true, y=*" $ do
        property $ \(PzTruish x) y -> do
            _and x y `shouldBe` y