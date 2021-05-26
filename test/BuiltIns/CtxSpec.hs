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
        -- generic
        M.lookup pzSymbTypeOf builtInCtx `shouldBe` Just pzTypeOf
        M.lookup pzSymbEq builtInCtx `shouldBe` Just pzEq
        M.lookup pzSymbLt builtInCtx `shouldBe` Just pzLt

        -- semi-generic
        M.lookup pzSymbIsEmpty builtInCtx `shouldBe` Just pzIsEmpty
        M.lookup pzSymbSize builtInCtx `shouldBe` Just pzSize

        -- numbers
        M.lookup pzSymbNum builtInCtx `shouldBe` Just pzNum
        M.lookup pzSymbAdd builtInCtx `shouldBe` Just pzAdd
        M.lookup pzSymbSub builtInCtx `shouldBe` Just pzSub
        M.lookup pzSymbMult builtInCtx `shouldBe` Just pzMult
        M.lookup pzSymbDiv builtInCtx `shouldBe` Just pzDiv
        M.lookup pzSymbRem builtInCtx `shouldBe` Just pzRem
        M.lookup pzSymbExp builtInCtx `shouldBe` Just pzExp
        M.lookup pzSymbLog builtInCtx `shouldBe` Just pzLog
        M.lookup pzSymbRound builtInCtx `shouldBe` Just pzRound
        M.lookup pzSymbFloor builtInCtx `shouldBe` Just pzFloor
        M.lookup pzSymbCeil builtInCtx `shouldBe` Just pzCeil
        M.lookup pzSymbTrunc builtInCtx `shouldBe` Just pzTrunc

        -- strings
        M.lookup pzSymbStr builtInCtx `shouldBe` Just pzStr
        M.lookup pzSymbSplit builtInCtx `shouldBe` Just pzSplit
        M.lookup pzSymbJoin builtInCtx `shouldBe` Just pzJoin

        -- symbols
        M.lookup pzSymbSymb builtInCtx `shouldBe` Just pzSymb
        M.lookup pzSymbNbrQuotes builtInCtx `shouldBe` Just pzNbrQuotes

        -- booleans
        M.lookup pzSymbFalse builtInCtx `shouldBe` Just pzSymbFalse
        M.lookup pzSymbTrue builtInCtx `shouldBe` Just pzSymbTrue
        M.lookup pzSymbNot builtInCtx `shouldBe` Just pzNot
        M.lookup pzSymbOr builtInCtx `shouldBe` Just pzOr
        M.lookup pzSymbAnd builtInCtx `shouldBe` Just pzAnd

        -- lists
        M.lookup pzSymbCons builtInCtx `shouldBe` Just pzCons
        M.lookup pzSymbHead builtInCtx `shouldBe` Just pzHead
        M.lookup pzSymbTail builtInCtx `shouldBe` Just pzTail

        -- dictionaries
        M.lookup pzSymbKeys builtInCtx `shouldBe` Just pzKeys
        M.lookup pzSymbAssocs builtInCtx `shouldBe` Just pzAssocs
        M.lookup pzSymbContains builtInCtx `shouldBe` Just pzContains
        M.lookup pzSymbGet builtInCtx `shouldBe` Just pzGet
        M.lookup pzSymbPut builtInCtx `shouldBe` Just pzPut
        M.lookup pzSymbDel builtInCtx `shouldBe` Just pzDel

        -- functions
        M.lookup pzSymbFunc builtInCtx `shouldBe` Just pzFunc
        M.lookup pzSymbGetImplCtx builtInCtx `shouldBe` Just pzGetImplCtx
        M.lookup pzSymbSetImplCtx builtInCtx `shouldBe` Just pzSetImplCtx
        M.lookup pzSymbGetExplCtx builtInCtx `shouldBe` Just pzGetExplCtx
        M.lookup pzSymbGetArgPass builtInCtx `shouldBe` Just pzGetArgPass
        M.lookup pzSymbGetArgs builtInCtx `shouldBe` Just pzGetArgs
        M.lookup pzSymbGetBody builtInCtx `shouldBe` Just pzGetBody