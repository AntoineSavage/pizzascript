
module BuiltIns.FuncImplsSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.FuncImpls
import BuiltIns.FuncValues
import Control.Monad
import Ops.BoolishSpec
import Ops.Func.ArgPass
import Ops.Func.FuncImpureArgs
import Ops.PzValSpec
import Symbs
import TestUtils
import Types.Func
import Types.Func.FuncArgs
import Types.Func.FuncBody
import Types.Func.FuncImpureArgsSpec
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
    _subSpec
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
    it "handles numbers" $ do
        property $ \n -> do
            _num (PzNum n) `shouldBe` Right (PzNum n)

    it "handles strings" $ do
        property $ \d s -> do
            _num (PzStr $ Str $ show d) `shouldBe` Right (PzNum $ Numb d)
            leftAsStr (_num (PzStr $ Str $ '_':s)) `shouldContain` "Call to function 'num"

    it "rejects the unit type" $ do
        let v = PzUnit
        _num v `shouldBe` Left ("Function 'num only supports numbers and strings\n was: " ++ show v)

    it "rejects symbols" $ do
        property $ \s -> do
            let v = PzSymb s
            _num v `shouldBe` Left ("Function 'num only supports numbers and strings\n was: " ++ show v)

    it "rejects lists" $ do
        property $ \(Few xs) -> do
            let v = PzList xs
            _num v `shouldBe` Left ("Function 'num only supports numbers and strings\n was: " ++ show v)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            _num v `shouldBe` Left ("Function 'num only supports numbers and strings\n was: " ++ show v)

    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            _num v `shouldBe` Left ("Function 'num only supports numbers and strings\n was: " ++ show v)

_addSpec :: Spec
_addSpec = describe "_add" $ do
    it "handles numbers" $ do
        property $ \d1 d2 -> do
            _add (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Right (PzNum $ Numb $ d1 + d2)

    it "rejects invalid result" $ do
        property $ \d2 -> do
            let d1 = 1/0
            _add (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Left ("Invalid addition: " ++ show d1 ++ " + " ++ show d2)

    it "rejects the unit type" $ do
        property $ \y -> do
            let x = PzUnit
            _add x y `shouldBe` Left ("Function 'add only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _add y x `shouldBe` Left ("Function 'add only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects strings" $ do
        property $ \s y -> do
            let x = PzStr s
            _add x y `shouldBe` Left ("Function 'add only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _add y x `shouldBe` Left ("Function 'add only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects symbols" $ do
        property $ \s y -> do
            let x = PzSymb s
            _add x y `shouldBe` Left ("Function 'add only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _add y x `shouldBe` Left ("Function 'add only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects lists" $ do
        property $ \(Few xs) y -> do
            let x = PzList xs
            _add x y `shouldBe` Left ("Function 'add only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _add y x `shouldBe` Left ("Function 'add only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) y -> do
            let x = PzDict d
            _add x y `shouldBe` Left ("Function 'add only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _add y x `shouldBe` Left ("Function 'add only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects functions" $ do
        property $ \(ArbDict d) f y -> do
            let x = PzFunc d f
            _add x y `shouldBe` Left ("Function 'add only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _add y x `shouldBe` Left ("Function 'add only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)
_subSpec :: Spec
_subSpec = describe "_sub" $ do
    it "handles numbers" $ do
        property $ \d1 d2 -> do
            _sub (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Right (PzNum $ Numb $ d1 - d2)

    it "rejects invalid result" $ do
        property $ \d2 -> do
            let d1 = 1/0
            _sub (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Left ("Invalid substraction: " ++ show d1 ++ " - " ++ show d2)

    it "rejects the unit type" $ do
        property $ \y -> do
            let x = PzUnit
            _sub x y `shouldBe` Left ("Function 'sub only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _sub y x `shouldBe` Left ("Function 'sub only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects strings" $ do
        property $ \s y -> do
            let x = PzStr s
            _sub x y `shouldBe` Left ("Function 'sub only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _sub y x `shouldBe` Left ("Function 'sub only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects symbols" $ do
        property $ \s y -> do
            let x = PzSymb s
            _sub x y `shouldBe` Left ("Function 'sub only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _sub y x `shouldBe` Left ("Function 'sub only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects lists" $ do
        property $ \(Few xs) y -> do
            let x = PzList xs
            _sub x y `shouldBe` Left ("Function 'sub only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _sub y x `shouldBe` Left ("Function 'sub only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) y -> do
            let x = PzDict d
            _sub x y `shouldBe` Left ("Function 'sub only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _sub y x `shouldBe` Left ("Function 'sub only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects functions" $ do
        property $ \(ArbDict d) f y -> do
            let x = PzFunc d f
            _sub x y `shouldBe` Left ("Function 'sub only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _sub y x `shouldBe` Left ("Function 'sub only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

_multSpec :: Spec
_multSpec = describe "_mult" $ do
    it "handles numbers" $ do
        property $ \d1 d2 -> do
            _mult (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Right (PzNum $ Numb $ d1 * d2)

    it "rejects invalid result" $ do
        property $ \d2 -> do
            let d1 = 1/0
            _mult (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Left ("Invalid multiplication: " ++ show d1 ++ " * " ++ show d2)

    it "rejects the unit type" $ do
        property $ \y -> do
            let x = PzUnit
            _mult x y `shouldBe` Left ("Function 'mult only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _mult y x `shouldBe` Left ("Function 'mult only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects strings" $ do
        property $ \s y -> do
            let x = PzStr s
            _mult x y `shouldBe` Left ("Function 'mult only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _mult y x `shouldBe` Left ("Function 'mult only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects symbols" $ do
        property $ \s y -> do
            let x = PzSymb s
            _mult x y `shouldBe` Left ("Function 'mult only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _mult y x `shouldBe` Left ("Function 'mult only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects lists" $ do
        property $ \(Few xs) y -> do
            let x = PzList xs
            _mult x y `shouldBe` Left ("Function 'mult only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _mult y x `shouldBe` Left ("Function 'mult only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) y -> do
            let x = PzDict d
            _mult x y `shouldBe` Left ("Function 'mult only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _mult y x `shouldBe` Left ("Function 'mult only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects functions" $ do
        property $ \(ArbDict d) f y -> do
            let x = PzFunc d f
            _mult x y `shouldBe` Left ("Function 'mult only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _mult y x `shouldBe` Left ("Function 'mult only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

_divSpec :: Spec
_divSpec = describe "_div" $ do
    it "handles numbers" $ do
        let d1 = 2
            d2 = 5
        _div (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Right (PzNum $ Numb $ d1 / d2)

    it "rejects invalid result" $ do
        let d1 = 1
            d2 = 0
        _div (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Left ("Invalid division: " ++ show d1 ++ " / " ++ show d2)

    it "rejects the unit type" $ do
        property $ \y -> do
            let x = PzUnit
            _div x y `shouldBe` Left ("Function 'div only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _div y x `shouldBe` Left ("Function 'div only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects strings" $ do
        property $ \s y -> do
            let x = PzStr s
            _div x y `shouldBe` Left ("Function 'div only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _div y x `shouldBe` Left ("Function 'div only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects symbols" $ do
        property $ \s y -> do
            let x = PzSymb s
            _div x y `shouldBe` Left ("Function 'div only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _div y x `shouldBe` Left ("Function 'div only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects lists" $ do
        property $ \(Few xs) y -> do
            let x = PzList xs
            _div x y `shouldBe` Left ("Function 'div only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _div y x `shouldBe` Left ("Function 'div only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) y -> do
            let x = PzDict d
            _div x y `shouldBe` Left ("Function 'div only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _div y x `shouldBe` Left ("Function 'div only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects functions" $ do
        property $ \(ArbDict d) f y -> do
            let x = PzFunc d f
            _div x y `shouldBe` Left ("Function 'div only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _div y x `shouldBe` Left ("Function 'div only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

_remSpec :: Spec
_remSpec = describe "_rem" $ do
    it "handles numbers" $ do
        let d1 = 2
            d2 = 5
        _rem (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ truncate d1 `rem` truncate d2)

    it "rejects invalid result" $ do
        let d1 = 1
            d2 = 0
        _rem (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Left ("Invalid remainder: " ++ show d1 ++ " % " ++ show d2)

    it "rejects the unit type" $ do
        property $ \y -> do
            let x = PzUnit
            _rem x y `shouldBe` Left ("Function 'rem only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _rem y x `shouldBe` Left ("Function 'rem only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects strings" $ do
        property $ \s y -> do
            let x = PzStr s
            _rem x y `shouldBe` Left ("Function 'rem only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _rem y x `shouldBe` Left ("Function 'rem only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects symbols" $ do
        property $ \s y -> do
            let x = PzSymb s
            _rem x y `shouldBe` Left ("Function 'rem only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _rem y x `shouldBe` Left ("Function 'rem only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects lists" $ do
        property $ \(Few xs) y -> do
            let x = PzList xs
            _rem x y `shouldBe` Left ("Function 'rem only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _rem y x `shouldBe` Left ("Function 'rem only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) y -> do
            let x = PzDict d
            _rem x y `shouldBe` Left ("Function 'rem only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _rem y x `shouldBe` Left ("Function 'rem only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects functions" $ do
        property $ \(ArbDict d) f y -> do
            let x = PzFunc d f
            _rem x y `shouldBe` Left ("Function 'rem only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _rem y x `shouldBe` Left ("Function 'rem only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

_expSpec :: Spec
_expSpec = describe "_exp" $ do
    it "handles numbers" $ do
        let d1 = 2
            d2 = 5
        _exp (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Right (PzNum $ Numb $ d1 ** d2)

    it "rejects invalid result" $ do
        let d1 = 1/0
            d2 = 1
        _exp (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Left ("Invalid exponentiation: " ++ show d1 ++ " ^ " ++ show d2)

    it "rejects the unit type" $ do
        property $ \y -> do
            let x = PzUnit
            _exp x y `shouldBe` Left ("Function 'exp only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _exp y x `shouldBe` Left ("Function 'exp only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects strings" $ do
        property $ \s y -> do
            let x = PzStr s
            _exp x y `shouldBe` Left ("Function 'exp only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _exp y x `shouldBe` Left ("Function 'exp only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects symbols" $ do
        property $ \s y -> do
            let x = PzSymb s
            _exp x y `shouldBe` Left ("Function 'exp only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _exp y x `shouldBe` Left ("Function 'exp only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects lists" $ do
        property $ \(Few xs) y -> do
            let x = PzList xs
            _exp x y `shouldBe` Left ("Function 'exp only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _exp y x `shouldBe` Left ("Function 'exp only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) y -> do
            let x = PzDict d
            _exp x y `shouldBe` Left ("Function 'exp only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _exp y x `shouldBe` Left ("Function 'exp only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects functions" $ do
        property $ \(ArbDict d) f y -> do
            let x = PzFunc d f
            _exp x y `shouldBe` Left ("Function 'exp only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _exp y x `shouldBe` Left ("Function 'exp only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

_logSpec :: Spec
_logSpec = describe "_log" $ do
    it "handles numbers" $ do
        let d1 = 2
            d2 = 5
        _log (PzNum $ Numb $ d1 + 1) (PzNum $ Numb d2) `shouldBe` Right (PzNum $ Numb $ logBase (d1 + 1) d2)

    it "rejects invalid result" $ do
        let d1 = 1/0
            d2 = 0
        _log (PzNum $ Numb d1) (PzNum $ Numb d2) `shouldBe` Left ("Invalid logarithm: log (base " ++ show d1 ++ ") " ++ show d2)

    it "rejects the unit type" $ do
        property $ \y -> do
            let x = PzUnit
            _log x y `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _log y x `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects strings" $ do
        property $ \s y -> do
            let x = PzStr s
            _log x y `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _log y x `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects symbols" $ do
        property $ \s y -> do
            let x = PzSymb s
            _log x y `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _log y x `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects lists" $ do
        property $ \(Few xs) y -> do
            let x = PzList xs
            _log x y `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _log y x `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) y -> do
            let x = PzDict d
            _log x y `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _log y x `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

    it "rejects functions" $ do
        property $ \(ArbDict d) f y -> do
            let x = PzFunc d f
            _log x y `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
            _log y x `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

_roundSpec :: Spec
_roundSpec = describe "_round" $ do
    it "handles numbers" $ do
        property $ \d -> do
            _round (PzNum $ Numb d) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ round $ d)

    it "rejects the unit type" $ do
        let v = PzUnit
        _round v `shouldBe` Left ("Function 'round only supports numbers\n was: " ++ show v)

    it "rejects strings" $ do
        property $ \s -> do
            let v = PzStr s
            _round v `shouldBe` Left ("Function 'round only supports numbers\n was: " ++ show v)

    it "rejects symbols" $ do
        property $ \s -> do
            let v = PzSymb s
            _round v `shouldBe` Left ("Function 'round only supports numbers\n was: " ++ show v)

    it "rejects lists" $ do
        property $ \(Few vs) -> do
            let v = PzList vs
            _round v `shouldBe` Left ("Function 'round only supports numbers\n was: " ++ show v)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            _round v `shouldBe` Left ("Function 'round only supports numbers\n was: " ++ show v)

    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            _round v `shouldBe` Left ("Function 'round only supports numbers\n was: " ++ show v)

_floorSpec :: Spec
_floorSpec = describe "_floor" $ do
    it "handles numbers" $ do
        property $ \d -> do
            _floor (PzNum $ Numb d) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ floor $ d)

    it "rejects the unit type" $ do
        let v = PzUnit
        _floor v `shouldBe` Left ("Function 'floor only supports numbers\n was: " ++ show v)

    it "rejects strings" $ do
        property $ \s -> do
            let v = PzStr s
            _floor v `shouldBe` Left ("Function 'floor only supports numbers\n was: " ++ show v)

    it "rejects symbols" $ do
        property $ \s -> do
            let v = PzSymb s
            _floor v `shouldBe` Left ("Function 'floor only supports numbers\n was: " ++ show v)

    it "rejects lists" $ do
        property $ \(Few vs) -> do
            let v = PzList vs
            _floor v `shouldBe` Left ("Function 'floor only supports numbers\n was: " ++ show v)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            _floor v `shouldBe` Left ("Function 'floor only supports numbers\n was: " ++ show v)

    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            _floor v `shouldBe` Left ("Function 'floor only supports numbers\n was: " ++ show v)

_ceilSpec :: Spec
_ceilSpec = describe "_ceil" $ do
    it "handles numbers" $ do
        property $ \d -> do
            _ceil (PzNum $ Numb d) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ ceiling $ d)

    it "rejects the unit type" $ do
        let v = PzUnit
        _ceil v `shouldBe` Left ("Function 'ceil only supports numbers\n was: " ++ show v)

    it "rejects strings" $ do
        property $ \s -> do
            let v = PzStr s
            _ceil v `shouldBe` Left ("Function 'ceil only supports numbers\n was: " ++ show v)

    it "rejects symbols" $ do
        property $ \s -> do
            let v = PzSymb s
            _ceil v `shouldBe` Left ("Function 'ceil only supports numbers\n was: " ++ show v)

    it "rejects lists" $ do
        property $ \(Few vs) -> do
            let v = PzList vs
            _ceil v `shouldBe` Left ("Function 'ceil only supports numbers\n was: " ++ show v)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            _ceil v `shouldBe` Left ("Function 'ceil only supports numbers\n was: " ++ show v)

    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            _ceil v `shouldBe` Left ("Function 'ceil only supports numbers\n was: " ++ show v)

_truncSpec :: Spec
_truncSpec = describe "_trunc" $ do
    it "handles numbers" $ do
        property $ \d -> do
            _trunc (PzNum $ Numb d) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ truncate $ d)

    it "rejects the unit type" $ do
        let v = PzUnit
        _trunc v `shouldBe` Left ("Function 'trunc only supports numbers\n was: " ++ show v)

    it "rejects strings" $ do
        property $ \s -> do
            let v = PzStr s
            _trunc v `shouldBe` Left ("Function 'trunc only supports numbers\n was: " ++ show v)

    it "rejects symbols" $ do
        property $ \s -> do
            let v = PzSymb s
            _trunc v `shouldBe` Left ("Function 'trunc only supports numbers\n was: " ++ show v)

    it "rejects lists" $ do
        property $ \(Few vs) -> do
            let v = PzList vs
            _trunc v `shouldBe` Left ("Function 'trunc only supports numbers\n was: " ++ show v)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            _trunc v `shouldBe` Left ("Function 'trunc only supports numbers\n was: " ++ show v)

    it "rejects functions" $ do
        property $ \(ArbDict d) f -> do
            let v = PzFunc d f
            _trunc v `shouldBe` Left ("Function 'trunc only supports numbers\n was: " ++ show v)

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
    it "handles functions" $ do
        property $ \ia -> do
            let r = case getExplCtx ia of
                    Just ec -> PzSymb ec
                    _       -> PzUnit
            _getExplCtx (PzFunc undefined $ Func ia undefined undefined) `shouldBe` Right r

    it "rejects the unit type" $ do
        let v = PzUnit
        _getExplCtx v `shouldBe` Left ("Function 'get_expl_ctx only supports functions\n was: " ++ show v)

    it "rejects numbers" $ do
        property $ \n -> do
            let v = PzNum n
            _getExplCtx v `shouldBe` Left ("Function 'get_expl_ctx only supports functions\n was: " ++ show v)

    it "rejects strings" $ do
        property $ \s -> do
            let v = PzStr s
            _getExplCtx v `shouldBe` Left ("Function 'get_expl_ctx only supports functions\n was: " ++ show v)

    it "rejects symbols" $ do
        property $ \s -> do
            let v = PzSymb s
            _getExplCtx v `shouldBe` Left ("Function 'get_expl_ctx only supports functions\n was: " ++ show v)

    it "rejects lists" $ do
        property $ \(Few xs) -> do
            let v = PzList xs
            _getExplCtx v `shouldBe` Left ("Function 'get_expl_ctx only supports functions\n was: " ++ show v)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            _getExplCtx v `shouldBe` Left ("Function 'get_expl_ctx only supports functions\n was: " ++ show v)

_getArgPassSpec :: Spec
_getArgPassSpec = describe "_getArgPass" $ do
    it "handles functions" $ do
        property $ \ia -> do
            _getArgPass (PzFunc undefined $ Func ia undefined undefined) `shouldBe` Right (PzSymb $ argPassToSymb $ getArgPass ia)

    it "rejects the unit type" $ do
        let v = PzUnit
        _getArgPass v `shouldBe` Left ("Function 'get_arg_pass only supports functions\n was: " ++ show v)

    it "rejects numbers" $ do
        property $ \n -> do
            let v = PzNum n
            _getArgPass v `shouldBe` Left ("Function 'get_arg_pass only supports functions\n was: " ++ show v)

    it "rejects strings" $ do
        property $ \s -> do
            let v = PzStr s
            _getArgPass v `shouldBe` Left ("Function 'get_arg_pass only supports functions\n was: " ++ show v)

    it "rejects symbols" $ do
        property $ \s -> do
            let v = PzSymb s
            _getArgPass v `shouldBe` Left ("Function 'get_arg_pass only supports functions\n was: " ++ show v)

    it "rejects lists" $ do
        property $ \(Few xs) -> do
            let v = PzList xs
            _getArgPass v `shouldBe` Left ("Function 'get_arg_pass only supports functions\n was: " ++ show v)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            _getArgPass v `shouldBe` Left ("Function 'get_arg_pass only supports functions\n was: " ++ show v)

_getArgsSpec :: Spec
_getArgsSpec = describe "_getArgs" $ do
    it "handles functions (varia)" $ do
        property $ \s -> do
            _getArgs (PzFunc undefined $ Func undefined (ArgsVaria s) undefined) `shouldBe` Right (PzSymb s)

    it "handles functions (arity)" $ do
        property $ \ss -> do
            _getArgs (PzFunc undefined $ Func undefined (ArgsArity ss) undefined) `shouldBe` Right (PzList $ map PzSymb ss)

    it "rejects the unit type" $ do
        let v = PzUnit
        _getArgs v `shouldBe` Left ("Function 'get_args only supports functions\n was: " ++ show v)

    it "rejects numbers" $ do
        property $ \n -> do
            let v = PzNum n
            _getArgs v `shouldBe` Left ("Function 'get_args only supports functions\n was: " ++ show v)

    it "rejects strings" $ do
        property $ \s -> do
            let v = PzStr s
            _getArgs v `shouldBe` Left ("Function 'get_args only supports functions\n was: " ++ show v)

    it "rejects symbols" $ do
        property $ \s -> do
            let v = PzSymb s
            _getArgs v `shouldBe` Left ("Function 'get_args only supports functions\n was: " ++ show v)

    it "rejects lists" $ do
        property $ \(Few xs) -> do
            let v = PzList xs
            _getArgs v `shouldBe` Left ("Function 'get_args only supports functions\n was: " ++ show v)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            _getArgs v `shouldBe` Left ("Function 'get_args only supports functions\n was: " ++ show v)

_getBodySpec :: Spec
_getBodySpec = describe "_getBody" $ do
    it "handles functions (built-in)" $ do
        property $ \s -> do
            _getBody (PzFunc undefined $ Func undefined undefined $ BodyBuiltIn s) `shouldBe` Right (PzSymb s)

    it "handles functions (custom)" $ do
        property $ \x xs -> do
            _getBody (PzFunc undefined $ Func undefined undefined $ BodyCustom x xs) `shouldBe` Right (PzList $ x:xs)

    it "rejects the unit type" $ do
        let v = PzUnit
        _getBody v `shouldBe` Left ("Function 'get_body only supports functions\n was: " ++ show v)

    it "rejects numbers" $ do
        property $ \n -> do
            let v = PzNum n
            _getBody v `shouldBe` Left ("Function 'get_body only supports functions\n was: " ++ show v)

    it "rejects strings" $ do
        property $ \s -> do
            let v = PzStr s
            _getBody v `shouldBe` Left ("Function 'get_body only supports functions\n was: " ++ show v)

    it "rejects symbols" $ do
        property $ \s -> do
            let v = PzSymb s
            _getBody v `shouldBe` Left ("Function 'get_body only supports functions\n was: " ++ show v)

    it "rejects lists" $ do
        property $ \(Few xs) -> do
            let v = PzList xs
            _getBody v `shouldBe` Left ("Function 'get_body only supports functions\n was: " ++ show v)

    it "rejects dictionaires" $ do
        property $ \(ArbDict d) -> do
            let v = PzDict d
            _getBody v `shouldBe` Left ("Function 'get_body only supports functions\n was: " ++ show v)