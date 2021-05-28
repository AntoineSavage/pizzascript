module BuiltIns.CtxSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as M

import BuiltIns.Ctx
import BuiltIns.FuncValues
import Symbs
import Types.PzVal

spec :: Spec
spec = do
    builtInCtxSpec

builtInCtxSpec :: Spec
builtInCtxSpec = describe "builtInCtx" $ do
    it "has the expected size" $ do
        M.size builtInCtx `shouldBe` 43

    it "contains the required keys" $ do
        let get k = M.lookup (DictKey k)

        -- generic
        get pzSymbTypeOf builtInCtx `shouldBe` Just pzTypeOf
        get pzSymbEq builtInCtx `shouldBe` Just pzEq
        get pzSymbLt builtInCtx `shouldBe` Just pzLt

        -- semi-generic
        get pzSymbIsEmpty builtInCtx `shouldBe` Just pzIsEmpty
        get pzSymbSize builtInCtx `shouldBe` Just pzSize

        -- numbers
        get pzSymbNum builtInCtx `shouldBe` Just pzNum
        get pzSymbAdd builtInCtx `shouldBe` Just pzAdd
        get pzSymbSub builtInCtx `shouldBe` Just pzSub
        get pzSymbMult builtInCtx `shouldBe` Just pzMult
        get pzSymbDiv builtInCtx `shouldBe` Just pzDiv
        get pzSymbRem builtInCtx `shouldBe` Just pzRem
        get pzSymbExp builtInCtx `shouldBe` Just pzExp
        get pzSymbLog builtInCtx `shouldBe` Just pzLog
        get pzSymbRound builtInCtx `shouldBe` Just pzRound
        get pzSymbFloor builtInCtx `shouldBe` Just pzFloor
        get pzSymbCeil builtInCtx `shouldBe` Just pzCeil
        get pzSymbTrunc builtInCtx `shouldBe` Just pzTrunc

        -- strings
        get pzSymbStr builtInCtx `shouldBe` Just pzStr
        get pzSymbSplit builtInCtx `shouldBe` Just pzSplit
        get pzSymbJoin builtInCtx `shouldBe` Just pzJoin

        -- symbols
        get pzSymbSymb builtInCtx `shouldBe` Just pzSymb
        get pzSymbNbrQuotes builtInCtx `shouldBe` Just pzNbrQuotes

        -- booleans
        get pzSymbFalse builtInCtx `shouldBe` Just pzSymbFalse
        get pzSymbTrue builtInCtx `shouldBe` Just pzSymbTrue
        get pzSymbNot builtInCtx `shouldBe` Just pzNot
        get pzSymbOr builtInCtx `shouldBe` Just pzOr
        get pzSymbAnd builtInCtx `shouldBe` Just pzAnd

        -- lists
        get pzSymbCons builtInCtx `shouldBe` Just pzCons
        get pzSymbHead builtInCtx `shouldBe` Just pzHead
        get pzSymbTail builtInCtx `shouldBe` Just pzTail

        -- dictionaries
        get pzSymbKeys builtInCtx `shouldBe` Just pzKeys
        get pzSymbAssocs builtInCtx `shouldBe` Just pzAssocs
        get pzSymbContains builtInCtx `shouldBe` Just pzContains
        get pzSymbGet builtInCtx `shouldBe` Just pzGet
        get pzSymbPut builtInCtx `shouldBe` Just pzPut
        get pzSymbDel builtInCtx `shouldBe` Just pzDel

        -- functions
        get pzSymbFunc builtInCtx `shouldBe` Just pzFunc
        get pzSymbGetImplCtx builtInCtx `shouldBe` Just pzGetImplCtx
        get pzSymbSetImplCtx builtInCtx `shouldBe` Just pzSetImplCtx
        get pzSymbGetExplCtx builtInCtx `shouldBe` Just pzGetExplCtx
        get pzSymbGetArgPass builtInCtx `shouldBe` Just pzGetArgPass
        get pzSymbGetArgs builtInCtx `shouldBe` Just pzGetArgs
        get pzSymbGetBody builtInCtx `shouldBe` Just pzGetBody