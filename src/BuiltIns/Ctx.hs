module BuiltIns.Ctx where

import qualified Data.Map as M

import BuiltIns.FuncValues
import Types.PzVal ( Dict, DictKey(..), PzVal(..) )
import Symbs
import Data.Bifunctor ( Bifunctor(first) )

builtInCtx :: Dict
builtInCtx = M.fromList $ map (Data.Bifunctor.first DictKey)
    [
    -- generic
      (pzSymbTypeOf, pzTypeOf)
    , (pzSymbEq, pzEq)
    , (pzSymbLt, pzLt)

    -- semi-generic
    , (pzSymbIsEmpty, pzIsEmpty)
    , (pzSymbSize, pzSize)

    -- numbers
    , (pzSymbNum, pzNum)
    , (pzSymbAdd, pzAdd)
    , (pzSymbSub, pzSub)
    , (pzSymbMult, pzMult)
    , (pzSymbDiv, pzDiv)
    , (pzSymbRem, pzRem)
    , (pzSymbExp, pzExp)
    , (pzSymbLog, pzLog)
    , (pzSymbRound, pzRound)
    , (pzSymbFloor, pzFloor)
    , (pzSymbCeil, pzCeil)
    , (pzSymbTrunc, pzTrunc)

    -- strings
    , (pzSymbStr, pzStr)
    , (pzSymbSplit, pzSplit)
    , (pzSymbJoin, pzJoin)

    -- symbols
    , (pzSymbSymb, pzSymb)
    , (pzSymbNbrQuotes, pzNbrQuotes)

    -- booleans
    , (pzSymbFalse, pzSymbFalse)
    , (pzSymbTrue, pzSymbTrue)
    , (pzSymbNot, pzNot)
    , (pzSymbOr, pzOr)
    , (pzSymbAnd, pzAnd)

    -- lists
    , (pzSymbCons, pzCons)
    , (pzSymbHead, pzHead)
    , (pzSymbTail, pzTail)

    -- dictionaries
    , (pzSymbKeys, pzKeys)
    , (pzSymbAssocs, pzAssocs)
    , (pzSymbContains, pzContains)
    , (pzSymbGet, pzGet)
    , (pzSymbPut, pzPut)
    , (pzSymbDel, pzDel)

    -- functions
    , (pzSymbFunc, pzFunc)
    , (pzSymbGetImplCtx, pzGetImplCtx)
    , (pzSymbSetImplCtx, pzSetImplCtx)
    , (pzSymbGetExplCtx, pzGetExplCtx)
    , (pzSymbGetArgPass, pzGetArgPass)
    , (pzSymbGetArgs, pzGetArgs)
    , (pzSymbGetBody, pzGetBody)
    ]