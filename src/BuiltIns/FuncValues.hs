module BuiltIns.FuncValues where

import qualified Data.Map as M

import Symbs
import Types.Func ( Func(..) )
import Types.Func.ArgPass ( ArgPass(..) )
import Types.Func.FuncArgs ( FuncArgs(..) )
import Types.Func.FuncBody ( FuncBody(..) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Types.PzVal ( Evaled, PzVal(..) )
import Types.Symb ( Symb )

-- generic
pzTypeOf :: PzVal Evaled
pzTypeOf = func1 symbTypeOf

pzEq :: PzVal Evaled
pzEq = func2 symbEq

pzLt :: PzVal Evaled
pzLt = func2 symbLt

-- semi-generic
pzIsEmpty :: PzVal Evaled
pzIsEmpty = func1 symbIsEmpty

pzSize :: PzVal Evaled
pzSize = func1 symbSize

-- numbers
pzNum :: PzVal Evaled
pzNum = func1 symbNum

pzAdd :: PzVal Evaled
pzAdd = func2 symbAdd

pzSub :: PzVal Evaled
pzSub = func2 symbSub

pzMult :: PzVal Evaled
pzMult = func2 symbMult

pzDiv :: PzVal Evaled
pzDiv = func2 symbDiv

pzRem :: PzVal Evaled
pzRem = func2 symbRem

pzExp :: PzVal Evaled
pzExp = func2 symbExp

pzLog :: PzVal Evaled
pzLog = func2 symbLog

pzRound :: PzVal Evaled
pzRound = func1 symbRound

pzFloor :: PzVal Evaled
pzFloor = func1 symbFloor

pzCeil :: PzVal Evaled
pzCeil = func1 symbCeil

pzTrunc :: PzVal Evaled
pzTrunc = func1 symbTrunc

-- strings
pzStr :: PzVal Evaled
pzStr = funcVaria symbStr

pzSplit :: PzVal Evaled
pzSplit = func2 symbSplit

pzJoin :: PzVal Evaled
pzJoin = funcVaria symbJoin

-- symbols
pzSymb :: PzVal Evaled
pzSymb = func1 symbSymb

pzNbrQuotes :: PzVal Evaled
pzNbrQuotes = func1 symbNbrQuotes

-- booleans
pzNot :: PzVal Evaled
pzNot = func1 symbNot

pzOr :: PzVal Evaled
pzOr = func2 symbOr

pzAnd :: PzVal Evaled
pzAnd = func2 symbAnd

-- lists
pzCons :: PzVal Evaled
pzCons = func2 symbCons

pzHead :: PzVal Evaled
pzHead = func1 symbHead

pzTail :: PzVal Evaled
pzTail = func1 symbTail

-- dictionaries
pzKeys :: PzVal Evaled
pzKeys = func1 symbKeys

pzAssocs :: PzVal Evaled
pzAssocs = func1 symbAssocs

pzContains :: PzVal Evaled
pzContains = func2 symbContains

pzGet :: PzVal Evaled
pzGet = func2 symbGet

pzPut :: PzVal Evaled
pzPut = func3 symbPut

pzDel :: PzVal Evaled
pzDel = func2 symbDel

-- functions
pzFunc :: PzVal Evaled
pzFunc = PzFunc M.empty $ Func (Both Quote symbCtx) (ArgsVaria symbArgs) $ BodyBuiltIn symbFunc

pzGetImplCtx :: PzVal Evaled
pzGetImplCtx = func1 symbGetImplCtx

pzSetImplCtx :: PzVal Evaled
pzSetImplCtx = func2 symbSetImplCtx

pzGetExplCtx :: PzVal Evaled
pzGetExplCtx = func1 symbGetExplCtx

pzGetArgPass :: PzVal Evaled
pzGetArgPass = func1 symbGetArgPass

pzGetArgs :: PzVal Evaled
pzGetArgs = func1 symbGetArgs

pzGetBody :: PzVal Evaled
pzGetBody = func1 symbGetBody

-- Utils
func1 :: Symb -> PzVal Evaled
func1 = PzFunc M.empty . Func None (ArgsArity [symbX]) . BodyBuiltIn

func2 :: Symb -> PzVal Evaled
func2 = PzFunc M.empty . Func None (ArgsArity [symbX, symbY]) . BodyBuiltIn

func3 :: Symb -> PzVal Evaled
func3 = PzFunc M.empty . Func None (ArgsArity [symbX, symbY, symbZ]) . BodyBuiltIn

funcVaria :: Symb -> PzVal Evaled
funcVaria = PzFunc M.empty . Func None (ArgsVaria symbArgs) . BodyBuiltIn