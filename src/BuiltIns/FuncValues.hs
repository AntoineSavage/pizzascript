module BuiltIns.FuncValues where

import qualified Data.Map as M

import Symbs
import Types.Func ( Func(..) )
import Types.Func.ArgPass ( ArgPass(..) )
import Types.Func.FuncArgs ( FuncArgs(..) )
import Types.Func.FuncBody ( FuncBody(..) )
import Types.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Types.PzVal ( PzVal(..) )
import Types.Symb ( Symb )

-- generic
pzTypeOf :: PzVal
pzTypeOf = func1 symbTypeOf

pzEq :: PzVal
pzEq = func2 symbEq

pzLt :: PzVal
pzLt = func2 symbLt

-- semi-generic
pzIsEmpty :: PzVal
pzIsEmpty = func1 symbIsEmpty

pzSize :: PzVal
pzSize = func1 symbSize

-- numbers
pzNum :: PzVal
pzNum = func1 symbNum

pzAdd :: PzVal
pzAdd = func2 symbAdd

pzSub :: PzVal
pzSub = func2 symbSub

pzMult :: PzVal
pzMult = func2 symbMult

pzDiv :: PzVal
pzDiv = func2 symbDiv

pzRem :: PzVal
pzRem = func2 symbRem

pzExp :: PzVal
pzExp = func2 symbExp

pzLog :: PzVal
pzLog = func2 symbLog

pzRound :: PzVal
pzRound = func1 symbRound

pzFloor :: PzVal
pzFloor = func1 symbFloor

pzCeil :: PzVal
pzCeil = func1 symbCeil

pzTrunc :: PzVal
pzTrunc = func1 symbTrunc

-- strings
pzStr :: PzVal
pzStr = funcVaria symbStr

pzSplit :: PzVal
pzSplit = func2 symbSplit

pzJoin :: PzVal
pzJoin = funcVaria symbJoin

-- symbols
pzSymb :: PzVal
pzSymb = func1 symbSymb

pzNbrQuotes :: PzVal
pzNbrQuotes = func1 symbNbrQuotes

-- booleans
pzNot :: PzVal
pzNot = func1 symbNot

pzOr :: PzVal
pzOr = func2 symbOr

pzAnd :: PzVal
pzAnd = func2 symbAnd

-- lists
pzCons :: PzVal
pzCons = func2 symbCons

pzHead :: PzVal
pzHead = func1 symbHead

pzTail :: PzVal
pzTail = func1 symbTail

-- dictionaries
pzKeys :: PzVal
pzKeys = func1 symbKeys

pzAssocs :: PzVal
pzAssocs = func1 symbAssocs

pzContains :: PzVal
pzContains = func2 symbContains

pzGet :: PzVal
pzGet = func2 symbGet

pzPut :: PzVal
pzPut = func3 symbPut

pzDel :: PzVal
pzDel = func2 symbDel

-- functions
pzFunc :: PzVal
pzFunc = PzFunc M.empty $ Func (Both Quote symbCtx) (ArgsVaria symbArgs) $ BodyBuiltIn symbFunc

pzGetImplCtx :: PzVal
pzGetImplCtx = func1 symbGetImplCtx

pzSetImplCtx :: PzVal
pzSetImplCtx = func2 symbSetImplCtx

pzGetExplCtx :: PzVal
pzGetExplCtx = func1 symbGetExplCtx

pzGetArgPass :: PzVal
pzGetArgPass = func1 symbGetArgPass

pzGetArgs :: PzVal
pzGetArgs = func1 symbGetArgs

pzGetBody :: PzVal
pzGetBody = func1 symbGetBody

-- Utils
func1 :: Symb -> PzVal
func1 = PzFunc M.empty . Func None (ArgsArity [symbX]) . BodyBuiltIn

func2 :: Symb -> PzVal
func2 = PzFunc M.empty . Func None (ArgsArity [symbX, symbY]) . BodyBuiltIn

func3 :: Symb -> PzVal
func3 = PzFunc M.empty . Func None (ArgsArity [symbX, symbY, symbZ]) . BodyBuiltIn

funcVaria :: Symb -> PzVal
funcVaria = PzFunc M.empty . Func None (ArgsVaria symbArgs) . BodyBuiltIn