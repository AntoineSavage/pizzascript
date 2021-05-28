module BuiltIns.FuncImplsSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

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
    -- generic
    _typeOfSpec
    _eqSpec
    _ltSpec

    -- semi-generic
    _isEmptySpec
    _sizeSpec

    -- numbers
    _numSpec
    _addSpec
    _multSpec
    _divSpec
    _remSpec
    _expSpec
    _logSpec
    _roundSpec
    _floorSpec
    _ceilSpec
    _truncSpec

    -- strings
    _strSpec
    _splitSpec
    _joinSpec

    -- symbols
    _symbSpec
    _nbrQuotesSpec

    -- booleans
    ifThenElseSpec
    _notSpec
    _orSpec
    _andSpec

    -- lists
    _consSpec
    _headSpec
    _tailSpec

    -- dictionaries
    _keysSpec
    _assocsSpec
    _containsSpec
    _getSpec
    _putSpec
    _delSpec

    -- functions
    _funcSpec
    _getImplCtxSpec
    _setImplCtxSpec
    _getExplCtxSpec
    _getArgPassSpec
    _getArgsSpec
    _getBodySpec

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
    it "compares using DictKey" $ do
        property $ \x y -> do
            _eq x x `shouldBe` pzSymbTrue
            _eq x y `shouldBe` if DictKey x == DictKey y then pzSymbTrue else pzSymbFalse

_ltSpec :: Spec
_ltSpec = describe "_lt" $ do
    it "compares using DictKey" $ do
        property $ \x y -> DictKey x /= DictKey y ==> do
            _lt x x `shouldBe` pzSymbFalse
            _lt x y `shouldBe` if DictKey x < DictKey y then pzSymbTrue else pzSymbFalse

_isEmptySpec :: Spec
_isEmptySpec = describe "_isEmpty" $ do
    it "handles strings" $ do
        property $ \s -> do
            _isEmpty (PzStr $ Str "") `shouldBe` Right pzSymbTrue
            _isEmpty (PzStr $ Str "abc") `shouldBe` Right pzSymbFalse
            _isEmpty (PzStr $ Str s) `shouldBe` Right (if null s then pzSymbTrue else pzSymbFalse)

    it "handles lists" $ do
        property $ \(Few xs) -> do
            _isEmpty (PzList []) `shouldBe` Right pzSymbTrue
            _isEmpty (PzList [PzUnit]) `shouldBe` Right pzSymbFalse
            _isEmpty (PzList xs) `shouldBe` Right (if null xs then pzSymbTrue else pzSymbFalse)

    it "handles dictionaries" $ do
        property $ \(ArbDict d) -> do
            _isEmpty (PzDict M.empty) `shouldBe` Right pzSymbTrue
            _isEmpty (PzDict $ M.fromList [(DictKey PzUnit, PzUnit)]) `shouldBe` Right pzSymbFalse
            _isEmpty (PzDict d) `shouldBe` Right (if M.null d then pzSymbTrue else pzSymbFalse)

    it "rejects the unit type" $ do
        let v = PzUnit
        _isEmpty v `shouldBe` Left ("Function 'is_empty only supports strings, lists and dictionaries\n was: " ++ show v)

    it "rejects numbers" $ do
        property $ \n -> do
            let v = PzNum n
            _isEmpty v `shouldBe` Left ("Function 'is_empty only supports strings, lists and dictionaries\n was: " ++ show v)

    it "rejects symbols" $ do
        property $ \s -> do
            let v = PzSymb s
            _isEmpty v `shouldBe` Left ("Function 'is_empty only supports strings, lists and dictionaries\n was: " ++ show v)

    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            _isEmpty v `shouldBe` Left ("Function 'is_empty only supports strings, lists and dictionaries\n was: " ++ show v)

_sizeSpec :: Spec
_sizeSpec = describe "_isEmpty" $ do
    it "handles strings" $ do
        property $ \s -> do
            _size (PzStr $ Str "") `shouldBe` Right (PzNum $ Numb 0)
            _size (PzStr $ Str "abc") `shouldBe` Right (PzNum $ Numb 3)
            _size (PzStr $ Str s) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ length s)

    it "handles lists" $ do
        property $ \(Few xs) -> do
            _size (PzList []) `shouldBe` Right (PzNum $ Numb 0)
            _size (PzList [PzUnit]) `shouldBe` Right (PzNum $ Numb 1)
            _size (PzList xs) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ length xs)

    it "handles dictionaries" $ do
        property $ \(ArbDict d) -> do
            _size (PzDict M.empty) `shouldBe` Right (PzNum $ Numb 0)
            _size (PzDict $ M.fromList [(DictKey PzUnit, PzUnit)]) `shouldBe` Right (PzNum $ Numb 1)
            _size (PzDict d) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ M.size d)

    it "rejects the unit type" $ do
        let v = PzUnit
        _size v `shouldBe` Left ("Function 'size only supports strings, lists and dictionaries\n was: " ++ show v)

    it "rejects numbers" $ do
        property $ \n -> do
            let v = PzNum n
            _size v `shouldBe` Left ("Function 'size only supports strings, lists and dictionaries\n was: " ++ show v)

    it "rejects symbols" $ do
        property $ \s -> do
            let v = PzSymb s
            _size v `shouldBe` Left ("Function 'size only supports strings, lists and dictionaries\n was: " ++ show v)

    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            _size v `shouldBe` Left ("Function 'size only supports strings, lists and dictionaries\n was: " ++ show v)

_numSpec :: Spec
_numSpec = describe "_num" $ do
    it "moos" $ pending

_addSpec :: Spec
_addSpec = describe "_add" $ do
    it "moos" $ pending

_subSpec :: Spec
_subSpec = describe "_sub" $ do
    it "moos" $ pending

_multSpec :: Spec
_multSpec = describe "_mult" $ do
    it "moos" $ pending

_divSpec :: Spec
_divSpec = describe "_div" $ do
    it "moos" $ pending

_remSpec :: Spec
_remSpec = describe "_rem" $ do
    it "moos" $ pending

_expSpec :: Spec
_expSpec = describe "_exp" $ do
    it "moos" $ pending

_logSpec :: Spec
_logSpec = describe "_log" $ do
    it "moos" $ pending

_roundSpec :: Spec
_roundSpec = describe "_round" $ do
    it "moos" $ pending

_floorSpec :: Spec
_floorSpec = describe "_floor" $ do
    it "moos" $ pending

_ceilSpec :: Spec
_ceilSpec = describe "_ceil" $ do
    it "moos" $ pending

_truncSpec :: Spec
_truncSpec = describe "_trunc" $ do
    it "moos" $ pending

_strSpec :: Spec
_strSpec = describe "_str" $ do
    it "moos" $ pending

_splitSpec :: Spec
_splitSpec = describe "_split" $ do
    it "moos" $ pending

_joinSpec :: Spec
_joinSpec = describe "_join" $ do
    it "moos" $ pending

_symbSpec :: Spec
_symbSpec = describe "_symb" $ do
    it "moos" $ pending

_nbrQuotesSpec :: Spec
_nbrQuotesSpec = describe "_nbrQuotes" $ do
    it "moos" $ pending

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

_consSpec :: Spec
_consSpec = describe "_cons" $ do
    it "moos" $ pending

_headSpec :: Spec
_headSpec = describe "_head" $ do
    it "moos" $ pending

_tailSpec :: Spec
_tailSpec = describe "_tail" $ do
    it "moos" $ pending

_keysSpec :: Spec
_keysSpec = describe "_keys" $ do
    it "moos" $ pending

_assocsSpec :: Spec
_assocsSpec = describe "_assocs" $ do
    it "moos" $ pending

_containsSpec :: Spec
_containsSpec = describe "_contains" $ do
    it "moos" $ pending

_getSpec :: Spec
_getSpec = describe "_get" $ do
    it "moos" $ pending

_putSpec :: Spec
_putSpec = describe "_put" $ do
    it "moos" $ pending

_delSpec :: Spec
_delSpec = describe "_del" $ do
    it "moos" $ pending

_funcSpec :: Spec
_funcSpec = describe "_func" $ do
    it "moos" $ pending

_getImplCtxSpec :: Spec
_getImplCtxSpec = describe "_getImplCtx" $ do
    it "moos" $ pending

_setImplCtxSpec :: Spec
_setImplCtxSpec = describe "_setImplCtx" $ do
    it "moos" $ pending

_getExplCtxSpec :: Spec
_getExplCtxSpec = describe "_getExplCtx" $ do
    it "moos" $ pending

_getArgPassSpec :: Spec
_getArgPassSpec = describe "_getArgPass" $ do
    it "moos" $ pending

_getArgsSpec :: Spec
_getArgsSpec = describe "_getArgs" $ do
    it "moos" $ pending

_getBodySpec :: Spec
_getBodySpec = describe "_getBody" $ do
    it "moos" $ pending