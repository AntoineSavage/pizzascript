
module BuiltIns.FuncImplsSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.FuncImpls
import BuiltIns.FuncValues
import Control.Monad
import Data.Either
import Data.List
import Eval
import Ops.BoolishSpec
import Ops.Func.ArgPass
import Ops.Func.FuncCustom
import Ops.Func.FuncImpureArgs
import Ops.Numb
import Ops.PzVal
import Ops.PzValSpec
import Ops.Symb
import Symbs
import TestUtils
import Types.Func
import Types.Func.FuncArgs
import Types.Func.FuncBody
import Types.Func.FuncCustomSpec
import Types.Func.FuncImpureArgsSpec
import Types.Numb
import Types.PzVal
import Types.PzValSpec
import Types.Str
import Utils

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

    -- utils
    actualSplitVsActualJoinSpec
    actualSplitSpec
    actualJoinSpec

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

    it "rejects other types" $ do
        property $ \n sym (ArbDict d) f -> do
            forM_ [PzUnit, PzNum n, PzSymb sym, PzFunc d f] $ \v -> do
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

    it "rejects other types" $ do
        property $ \n sym (ArbDict d) f -> do
            forM_ [PzUnit, PzNum n, PzSymb sym, PzFunc d f] $ \v -> do
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

    it "rejects other types" $ do
        property $ \sym (Few xs) (ArbDict d) f -> do
            forM_ [PzUnit, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \v -> do
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

    it "rejects other types" $ do
        property $ \s sym (Few xs) (ArbDict d) f y -> do
            forM_ [PzUnit, PzStr s, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \x -> do    
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

    it "rejects other types" $ do
        property $ \s sym (Few xs) (ArbDict d) f y -> do
            forM_ [PzUnit, PzStr s, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \x -> do
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

    it "rejects other types" $ do
        property $ \s sym (Few xs) (ArbDict d) f y -> do
            forM_ [PzUnit, PzStr s, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \x -> do
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

    it "rejects other types" $ do
        property $ \s sym (Few xs) (ArbDict d) f y -> do
            forM_ [PzUnit, PzStr s, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \x -> do
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

    it "rejects other types" $ do
        property $ \s sym (Few xs) (ArbDict d) f y -> do
            forM_ [PzUnit, PzStr s, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \x -> do
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

    it "rejects other types" $ do
        property $ \s sym (Few xs) (ArbDict d) f y -> do
            forM_ [PzUnit, PzStr s, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \x -> do
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

    it "rejects other types" $ do
        property $ \s sym (Few xs) (ArbDict d) f y -> do
            forM_ [PzUnit, PzStr s, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \x -> do
                _log x y `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show x ++ "\n and: " ++ show y)
                _log y x `shouldBe` Left ("Function 'log only supports numbers\n was: " ++ show y ++ "\n and: " ++ show x)

_roundSpec :: Spec
_roundSpec = describe "_round" $ do
    it "handles numbers" $ do
        property $ \d -> do
            _round (PzNum $ Numb d) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ round $ d)

    it "rejects other types" $ do
        property $ \s sym (Few xs) (ArbDict d) f -> do
            forM_ [PzUnit, PzStr s, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \v -> do
                _round v `shouldBe` Left ("Function 'round only supports numbers\n was: " ++ show v)

_floorSpec :: Spec
_floorSpec = describe "_floor" $ do
    it "handles numbers" $ do
        property $ \d -> do
            _floor (PzNum $ Numb d) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ floor $ d)

    it "rejects other types" $ do
        property $ \s sym (Few xs) (ArbDict d) f -> do
            forM_ [PzUnit, PzStr s, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \v -> do
                _floor v `shouldBe` Left ("Function 'floor only supports numbers\n was: " ++ show v)

_ceilSpec :: Spec
_ceilSpec = describe "_ceil" $ do
    it "handles numbers" $ do
        property $ \d -> do
            _ceil (PzNum $ Numb d) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ ceiling $ d)

    it "rejects other types" $ do
        property $ \s sym (Few xs) (ArbDict d) f -> do
            forM_ [PzUnit, PzStr s, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \v -> do
                _ceil v `shouldBe` Left ("Function 'ceil only supports numbers\n was: " ++ show v)

_truncSpec :: Spec
_truncSpec = describe "_trunc" $ do
    it "handles numbers" $ do
        property $ \d -> do
            _trunc (PzNum $ Numb d) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ truncate $ d)

    it "rejects other types" $ do
        property $ \s sym (Few xs) (ArbDict d) f -> do
            forM_ [PzUnit, PzStr s, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \v -> do
                _trunc v `shouldBe` Left ("Function 'trunc only supports numbers\n was: " ++ show v)

_strSpec :: Spec
_strSpec = describe "_str" $ do
    it "returns empty string for empty args" $ do
        _str [] `shouldBe` PzStr (Str "")

    it "unevals+unparses the unit type" $ do
        _str [PzUnit] `shouldBe` PzStr (Str "()")

    it "unparses number" $ do
        property $ \n -> do
            let v = PzNum n
            _str [v] `shouldBe` PzStr (Str $ unparseNumb n)

    it "returns string as-is" $ do
        property $ \s -> do
            let v = PzStr $ Str s
            _str [v] `shouldBe` PzStr (Str s)

    it "quotes+unparse symbol" $ do
        property $ \s -> do
            let v = PzSymb s
            _str [v] `shouldBe` PzStr (Str $ unparseSymb $ quoteSymb s)

    it "uneval+unparse list" $ do
        let v = PzList []
        _str [v] `shouldBe` PzStr (Str "[]")

    it "uneval+unparse dict" $ do
        let v = PzDict M.empty
        _str [v] `shouldBe` PzStr (Str "{}")

    it "uneval+unparse func" $ do
        property $ \ia s -> do
            let v = PzFunc undefined $ Func ia (ArgsVaria s) (BodyBuiltIn s)
            _str [v] `shouldBe` PzStr (Str $ unparseSymb s)

    it "concats multiple values" $ do
        property $ \n s sym ia -> do
            let vs =    [ PzUnit
                        , PzNum n
                        , PzStr $ Str s
                        , PzSymb sym
                        , PzList []
                        , PzDict M.empty
                        , PzFunc undefined $ Func ia (ArgsVaria sym) (BodyBuiltIn sym)
                        ]
                ss =    [ "()"
                        , unparseNumb n
                        , s
                        , unparseSymb (quoteSymb sym)
                        , "[]"
                        , "{}"
                        , unparseSymb sym
                        ]
            _str vs `shouldBe` PzStr (Str $ concat ss)

_splitSpec :: Spec
_splitSpec = describe "_split" $ do
    it "handles strings" $ do
        property $ \sep s -> do
            _split (PzStr $ Str sep) (PzStr $ Str s) `shouldBe` Right (PzList $ map (PzStr . Str) $ actualSplit sep s)

    it "rejects other types" $ do
        property $ \n sym (Few xs) (ArbDict d) f y -> do
            forM_ [PzUnit, PzNum n, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \x -> do
                _split x y `shouldBe` Left ("Function 'split only supports strings\n was: " ++ show x ++ "\n and: " ++ show y)
                _split y x `shouldBe` Left ("Function 'split only supports strings\n was: " ++ show y ++ "\n and: " ++ show x)

_joinSpec :: Spec
_joinSpec = describe "_join" $ do
    it "rejects less than two args" $ do
        property $ \v -> do
            _join [] `shouldBe` Left "Function 'join requires at least two arguments, but got zero"
            _join [v] `shouldBe` Left ("Function 'join requires at least two arguments, but got one\n was: " ++ show v)

    it "rejects non-string separator" $ do
        property $ \n sym (Few xs) (ArbDict d) f y -> do
            forM_ [PzUnit, PzNum n, PzSymb sym, PzList xs, PzDict d, PzFunc d f] $ \x -> do
                _join [x,y] `shouldBe` Left ("Function 'join only support strings (first arg)\n was: " ++ show x)

    it "rejects non-string or non-list second arg" $ do
        property $ \n s sym (ArbDict d) f -> do
            forM_ [PzUnit, PzNum n, PzSymb sym, PzDict d, PzFunc d f] $ \y -> do
                _join [PzStr $ Str s, y] `shouldBe` Left ("Function 'join only support strings or lists of strings (second or more arg)\n was: " ++ show y)

    it "rejects non-string or non-list third arg" $ do
        property $ \n s sym (ArbDict d) f -> do
            forM_ [PzUnit, PzNum n, PzSymb sym, PzDict d, PzFunc d f] $ \z -> do
                _join [PzStr $ Str s, PzList [PzStr $ Str s], z] `shouldBe` Left ("Function 'join only support strings or lists of strings (second or more arg)\n was: " ++ show z)

    it "joins strings (flat)" $ do
        property $ \sep s (Few ss) -> do
            let args = map (PzStr . Str) $ sep:s:ss
            _join args `shouldBe` Right (PzStr $ Str $ actualJoin sep $ s:ss)

    it "joins strings (deep)" $ do
        property $ \sep (Few ss1) (Few ss2) (Few ss3) -> do
            let sss = [ss1, ss2, ss3]
                args = [ PzStr $ Str sep, PzList $ map (PzList . map (PzStr . Str)) sss ]
            _join args `shouldBe` Right (PzStr $ Str $ actualJoin sep $ concat sss)

_symbSpec :: Spec
_symbSpec = describe "_symb" $ do
    it "handles strings" $ do
        property $ \s sym -> do
            _symb (PzStr $ Str $ unparseSymb sym) `shouldBe` Right (PzSymb sym)
            leftAsStr (_symb (PzStr $ Str $ '$':s)) `shouldContain` "Call to function 'symb"

    it "handles symbols" $ do
        property $ \s -> do
            _symb (PzSymb s) `shouldBe` Right (PzSymb s)

    it "rejects other types" $ do
        property $ \n (Few xs) (ArbDict d) f -> do
            forM_ [PzUnit, PzNum n, PzList xs, PzDict d, PzFunc d f] $ \v -> do
                _symb v `shouldBe` Left ("Function 'symb only supports strings and symbols\n was: " ++ show v)

_nbrQuotesSpec :: Spec
_nbrQuotesSpec = describe "_nbrQuotes" $ do
    it "handles symbols" $ do
        property $ \s -> do
            _nbrQuotes (PzSymb s) `shouldBe` Right (PzNum $ Numb $ fromIntegral $ getNbrQuotes s)

    it "rejects other types" $ do
        property $ \n s (Few xs) (ArbDict d) f -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzList xs, PzDict d, PzFunc d f] $ \v -> do
                _nbrQuotes v `shouldBe` Left ("Function 'nbr_quotes only supports symbols\n was: " ++ show v)

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
    it "handles lists (empty)" $ do
        property $ \x -> do
            _cons x (PzList []) `shouldBe` Right (PzList [x])

    it "handles lists (non-empty)" $ do
        property $ \x (Few xs) -> do
            _cons x (PzList xs) `shouldBe` Right (PzList $ x:xs)

    it "rejects other types" $ do
        property $ \n s sym (ArbDict d) f x -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzDict d, PzFunc d f] $ \y -> do
                _cons x y `shouldBe` Left ("Function 'cons only supports lists (second arg)\n was: " ++ show y)

_headSpec :: Spec
_headSpec = describe "_head" $ do
    it "handles lists (non-empty)" $ do
        property $ \x -> do
            _head (PzList $ x:undefined) `shouldBe` Right x

    it "handles lists (empty)" $ do
        let v = PzList []
        _head v `shouldBe` Left ("Function 'head only supports non-empty lists\n was: " ++ show v)

    it "rejects other types" $ do
        property $ \n s sym (ArbDict d) f -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzDict d, PzFunc d f] $ \v -> do
                _head v `shouldBe` Left ("Function 'head only supports non-empty lists\n was: " ++ show v)

_tailSpec :: Spec
_tailSpec = describe "_tail" $ do
    it "handles lists (non-empty)" $ do
        property $ \(Few xs) -> do
            _tail (PzList $ undefined:xs) `shouldBe` Right (PzList xs)

    it "handles lists (empty)" $ do
        let v = PzList []
        _tail v `shouldBe` Left ("Function 'tail only supports non-empty lists\n was: " ++ show v)

    it "rejects other types" $ do
        property $ \n s sym (ArbDict d) f -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzDict d, PzFunc d f] $ \v -> do
                _tail v `shouldBe` Left ("Function 'tail only supports non-empty lists\n was: " ++ show v)

_keysSpec :: Spec
_keysSpec = describe "_keys" $ do
    it "handles dictionaries" $ do
        property $ \(ArbDict d) -> do
            _keys (PzDict d) `shouldBe` Right (PzList $ map unDictKey $ M.keys d)

    it "rejects other types" $ do
        property $ \n s sym (Few xs) (ArbDict d) f -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzFunc d f] $ \v -> do
                _keys v `shouldBe` Left ("Function 'keys only supports dictionaries\n was: " ++ show v)

_assocsSpec :: Spec
_assocsSpec = describe "_assocs" $ do
    it "handles dictionaries" $ do
        property $ \(ArbDict d) -> do
            _assocs (PzDict d) `shouldBe` Right (PzList $ flip map (M.assocs d) $ \(DictKey k, v) -> PzList [k, v])

    it "rejects other types" $ do
        property $ \n s sym (Few xs) (ArbDict d) f -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzFunc d f] $ \v -> do
                _assocs v `shouldBe` Left ("Function 'assocs only supports dictionaries\n was: " ++ show v)

_containsSpec :: Spec
_containsSpec = describe "_contains" $ do
    it "handles dictionaries (found)" $ do
        property $ \k v (ArbDict d') -> do
            let d = M.insert (DictKey k) v d'
            _contains (PzDict d) k `shouldBe` Right pzSymbTrue

    it "handles dictionaries (not found)" $ do
        property $ \k (ArbDict d') -> do
            let d = M.delete (DictKey k) d'
            _contains (PzDict d) k `shouldBe` Right pzSymbFalse

    it "rejects other types" $ do
        property $ \n s sym (Few xs) (ArbDict d) f y -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzFunc d f] $ \x -> do
                _contains x y `shouldBe` Left ("Function 'contains only supports dictionaries (first arg)\n was: " ++ show x)

_getSpec :: Spec
_getSpec = describe "_get" $ do
    it "handles dictionaries (found)" $ do
        property $ \k v (ArbDict d') -> do
            let d = M.insert (DictKey k) v d'
            _get (PzDict d) k `shouldBe` Right v

    it "handles dictionaries (not found)" $ do
        property $ \k (ArbDict d') -> do
            let d = M.delete (DictKey k) d'
            _get (PzDict d) k `shouldBe` Right PzUnit

    it "rejects other types" $ do
        property $ \n s sym (Few xs) (ArbDict d) f y -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzFunc d f] $ \x -> do
                _get x y `shouldBe` Left ("Function 'get only supports dictionaries (first arg)\n was: " ++ show x)

_putSpec :: Spec
_putSpec = describe "_put" $ do
    it "handles dictionaries (not found and added)" $ do
        property $ \k v (ArbDict d') -> do
            let d = M.delete (DictKey k) d'
                dr = M.insert (DictKey k) v d'
            _put (PzDict d) k v `shouldBe` Right (PzDict dr)

    it "handles dictionaries (found and replaced)" $ do
        property $ \k v (ArbDict d') -> do
            let d = M.insert (DictKey k) PzUnit d'
                dr = M.insert (DictKey k) v d'
            _put (PzDict d) k v `shouldBe` Right (PzDict dr)

    it "rejects other types" $ do
        property $ \n s sym (Few xs) (ArbDict d) f y z -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzFunc d f] $ \x -> do
                _put x y z `shouldBe` Left ("Function 'put only supports dictionaries (first arg)\n was: " ++ show x)

_delSpec :: Spec
_delSpec = describe "_del" $ do
    it "handles dictionaries (found and removed)" $ do
        property $ \k v (ArbDict d') -> do
            let d = M.insert (DictKey k) v d'
                dr = M.delete (DictKey k) d'
            _del (PzDict d) k `shouldBe` Right (PzDict dr)

    it "handles dictionaries (not found)" $ do
        property $ \k (ArbDict d') -> do
            let d = M.delete (DictKey k) d'
            _del (PzDict d) k `shouldBe` Right (PzDict d)

    it "rejects other types" $ do
        property $ \n s sym (Few xs) (ArbDict d) f y -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzFunc d f] $ \x -> do
               _del x y `shouldBe` Left ("Function 'del only supports dictionaries (first arg)\n was: " ++ show x)

_funcSpec :: Spec
_funcSpec = describe "_func" $ do
    it "handles valid" $ do
        property $ \(ArbDict d) fc -> do
            let elems = unevalFuncCustom fc
                f = fromFuncCustom fc
            _func d elems `shouldBe` (Right $ PzList [PzDict d, PzFunc d f])

    it "handles invalid" $ do
        property $ \(ArbDict d) vs -> do
            let elems = PzUnit : vs
            isLeft (_func d elems) `shouldBe` True

_getImplCtxSpec :: Spec
_getImplCtxSpec = describe "_getImplCtx" $ do
    it "handles functions" $ do
        property $ \(ArbDict d) -> do
            _getImplCtx (PzFunc d undefined) `shouldBe` Right (PzDict d)

    it "rejects other types" $ do
        property $ \n s sym (Few xs) (ArbDict d) -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzDict d] $ \v -> do
                _getImplCtx v `shouldBe` Left ("Function 'get_impl_ctx only supports functions\n was: " ++ show v)

_setImplCtxSpec :: Spec
_setImplCtxSpec = describe "_setImplCtx" $ do
    it "handles functions" $ do
        property $ \(ArbDict d) f -> do
            _setImplCtx (PzFunc undefined f) (PzDict d) `shouldBe` Right (PzFunc d f)

    it "rejects other types (first arg)" $ do
        property $ \n s sym (Few xs) (ArbDict d) -> do
            let y = PzDict d
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzDict d] $ \x -> do
                _setImplCtx x y `shouldBe` Left ("Function 'set_impl_ctx only supports functions (first arg) and dictionaries (second arg)\n was: " ++ show x ++ "\n and: " ++ show y)

    it "rejects other types (second arg)" $ do
        property $ \n s sym (Few xs) (ArbDict d) f -> do
            let x = PzFunc d f
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzFunc d f] $ \y -> do
                _setImplCtx x y `shouldBe` Left ("Function 'set_impl_ctx only supports functions (first arg) and dictionaries (second arg)\n was: " ++ show x ++ "\n and: " ++ show y)

_getExplCtxSpec :: Spec
_getExplCtxSpec = describe "_getExplCtx" $ do
    it "handles functions" $ do
        property $ \ia -> do
            let r = case getExplCtx ia of
                    Just ec -> PzSymb ec
                    _       -> PzUnit
            _getExplCtx (PzFunc undefined $ Func ia undefined undefined) `shouldBe` Right r

    it "rejects other types" $ do
        property $ \n s sym (Few xs) (ArbDict d) -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzDict d] $ \v -> do
                _getExplCtx v `shouldBe` Left ("Function 'get_expl_ctx only supports functions\n was: " ++ show v)

_getArgPassSpec :: Spec
_getArgPassSpec = describe "_getArgPass" $ do
    it "handles functions" $ do
        property $ \ia -> do
            _getArgPass (PzFunc undefined $ Func ia undefined undefined) `shouldBe` Right (PzSymb $ argPassToSymb $ getArgPass ia)

    it "rejects other types" $ do
        property $ \n s sym (Few xs) (ArbDict d) -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzDict d] $ \v -> do
                _getArgPass v `shouldBe` Left ("Function 'get_arg_pass only supports functions\n was: " ++ show v)

_getArgsSpec :: Spec
_getArgsSpec = describe "_getArgs" $ do
    it "handles functions (varia)" $ do
        property $ \s -> do
            _getArgs (PzFunc undefined $ Func undefined (ArgsVaria s) undefined) `shouldBe` Right (PzSymb s)

    it "handles functions (arity)" $ do
        property $ \ss -> do
            _getArgs (PzFunc undefined $ Func undefined (ArgsArity ss) undefined) `shouldBe` Right (PzList $ map PzSymb ss)

    it "rejects other types" $ do
        property $ \n s sym (Few xs) (ArbDict d) -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzDict d] $ \v -> do
                _getArgs v `shouldBe` Left ("Function 'get_args only supports functions\n was: " ++ show v)

_getBodySpec :: Spec
_getBodySpec = describe "_getBody" $ do

    it "handles functions (built-in)" $ do
        property $ \s -> do
            _getBody (PzFunc undefined $ Func undefined undefined $ BodyBuiltIn s) `shouldBe` Right (PzSymb s)

    it "handles functions (custom)" $ do
        property $ \x xs -> do
            _getBody (PzFunc undefined $ Func undefined undefined $ BodyCustom x xs) `shouldBe` Right (PzList $ x:xs)

    it "rejects other types" $ do
        property $ \n s sym (Few xs) (ArbDict d) -> do
            forM_ [PzUnit, PzNum n, PzStr s, PzSymb sym, PzList xs, PzDict d] $ \v -> do
                _getBody v `shouldBe` Left ("Function 'get_body only supports functions\n was: " ++ show v)

actualSplitVsActualJoinSpec :: Spec
actualSplitVsActualJoinSpec = describe "actualSplit vs actualJoin" $ do
    it "composes actualSplit and actualJoin into id" $ do
        forM_ [",", ";", "# "] $ \sep -> do
            forM_ [["a","b","c"],["xyz"]] $ \ss -> do
                actualSplit sep (actualJoin sep ss) `shouldBe` ss

    it "composes actualSplit and actualJoin into id (empty input)" $ do
        forM_ [",", ";", "# "] $ \sep -> do
            actualSplit sep (actualJoin sep []) `shouldBe` []
            actualSplit sep (actualJoin sep [""]) `shouldBe` []
            actualSplit sep (actualJoin sep ["", ""]) `shouldBe` []
            actualSplit sep (actualJoin sep ["", "", ""]) `shouldBe` []

    it "composes actualSplit and actualJoin into id (empty sep)" $ do
        forM_   [ (["a","b","c"], ["a","b","c"])
                , (["xyz"], ["x","y","z"])
                , ([], [])
                , ([""], [])
                ] $ \(input, expected) -> do
            actualSplit "" (actualJoin "" input) `shouldBe` expected

actualSplitSpec :: Spec
actualSplitSpec = describe "actualSplit" $ do
    it "split empty separator to singleton strings" $ do
        actualSplit "" "" `shouldBe` []
        actualSplit "" "a" `shouldBe` ["a"]
        actualSplit "" "ab" `shouldBe` ["a", "b"]
        actualSplit "" "abc" `shouldBe` ["a", "b", "c"]

    it "splits one-char separator to empty" $ do
        actualSplit "a" "a" `shouldBe` []
        actualSplit "a" "" `shouldBe` []

    it "splits two-char separator to empty" $ do
        actualSplit "ab" "ab" `shouldBe` []
        actualSplit "ab" "" `shouldBe` []

    it "splits three-char separator to empty" $ do
        actualSplit "abc" "abc" `shouldBe` []
        actualSplit "abc" "" `shouldBe` []

    it "splits comma-separated string" $ do
        actualSplit "," ",abc,def,123,xyz," `shouldBe` ["abc","def","123","xyz"]

actualJoinSpec :: Spec
actualJoinSpec = describe "actualJoin" $ do
    it "joins no elems" $ do
        actualJoin undefined [] `shouldBe` []
    
    it "joins one elem" $ do
        property $ \e1 -> do
            actualJoin undefined [e1] `shouldBe` e1
    
    it "joins two elems" $ do
        property $ \sep e1 e2 -> do
            actualJoin sep [e1,e2] `shouldBe` e1 ++ sep ++ e2
    
    it "joins three elems" $ do
        property $ \sep e1 e2 e3 -> do
            actualJoin sep [e1,e2,e3] `shouldBe` e1 ++ sep ++ e2 ++ sep ++ e3
    
    it "joins N elems" $ do
        property $ \sep (Few es) -> do
            actualJoin sep es `shouldBe` intercalate sep es