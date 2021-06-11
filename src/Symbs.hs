module Symbs where

import Ops.Symb ( quoteSymb, symb )
import Types.PzVal ( PzVal(PzSymb) )
import Types.Symb ( Symb(..) )

-- All the following symbols correspond to quoted identifiers
-- i.e. identifiers that were just parsed
-- They also correspond to single-quoted symbols

-- generic
symbTypeOf :: Symb;             pzSymbTypeOf :: PzVal f
symbTypeOf = symb "type_of";    pzSymbTypeOf = PzSymb symbTypeOf

symbEq :: Symb;             pzSymbEq :: PzVal f
symbEq = symb "eq";         pzSymbEq = PzSymb symbEq

symbLt :: Symb;             pzSymbLt :: PzVal f
symbLt = symb "lt";         pzSymbLt = PzSymb symbLt

-- semi-generic
symbIsEmpty :: Symb;            pzSymbIsEmpty :: PzVal f
symbIsEmpty = symb "is_empty";  pzSymbIsEmpty = PzSymb symbIsEmpty

symbSize :: Symb;           pzSymbSize :: PzVal f
symbSize = symb "size";     pzSymbSize = PzSymb symbSize

-- numbers
symbNum :: Symb;            pzSymbNum :: PzVal f
symbNum = symb "num";       pzSymbNum = PzSymb symbNum

symbAdd :: Symb;            pzSymbAdd :: PzVal f
symbAdd = symb "add";       pzSymbAdd = PzSymb symbAdd

symbSub :: Symb;            pzSymbSub :: PzVal f
symbSub = symb "sub";       pzSymbSub = PzSymb symbSub

symbMult :: Symb;           pzSymbMult :: PzVal f
symbMult = symb "mult";     pzSymbMult = PzSymb symbMult

symbDiv :: Symb;            pzSymbDiv :: PzVal f
symbDiv = symb "div";       pzSymbDiv = PzSymb symbDiv

symbRem :: Symb;            pzSymbRem :: PzVal f
symbRem = symb "rem";       pzSymbRem = PzSymb symbRem

symbExp :: Symb;            pzSymbExp :: PzVal f
symbExp = symb "exp";       pzSymbExp = PzSymb symbExp

symbLog :: Symb;            pzSymbLog :: PzVal f
symbLog = symb "log";       pzSymbLog = PzSymb symbLog

symbRound :: Symb;          pzSymbRound :: PzVal f
symbRound = symb "round";   pzSymbRound = PzSymb symbRound

symbFloor :: Symb;          pzSymbFloor :: PzVal f
symbFloor = symb "floor";   pzSymbFloor = PzSymb symbFloor

symbCeil :: Symb;           pzSymbCeil :: PzVal f
symbCeil = symb "ceil";     pzSymbCeil = PzSymb symbCeil

symbTrunc :: Symb;          pzSymbTrunc :: PzVal f
symbTrunc = symb "trunc";   pzSymbTrunc = PzSymb symbTrunc

-- strings
symbStr :: Symb;            pzSymbStr :: PzVal f
symbStr = symb "str";       pzSymbStr = PzSymb symbStr

symbSplit :: Symb;          pzSymbSplit :: PzVal f
symbSplit = symb "split";   pzSymbSplit = PzSymb symbSplit

symbJoin :: Symb;           pzSymbJoin :: PzVal f
symbJoin = symb "join";     pzSymbJoin = PzSymb symbJoin

-- symbols
symbSymb :: Symb;           pzSymbSymb :: PzVal f
symbSymb = symb "symb";     pzSymbSymb = PzSymb symbSymb

symbNbrQuotes :: Symb;              pzSymbNbrQuotes :: PzVal f
symbNbrQuotes = symb "nbr_quotes";  pzSymbNbrQuotes = PzSymb symbNbrQuotes

-- booleans
symbFalse :: Symb;          pzSymbFalse :: PzVal f
symbFalse = symb "false";   pzSymbFalse = PzSymb symbFalse

symbTrue :: Symb;           pzSymbTrue :: PzVal f
symbTrue = symb "true";     pzSymbTrue = PzSymb symbTrue

symbNot :: Symb;            pzSymbNot :: PzVal f
symbNot = symb "not";       pzSymbNot = PzSymb symbNot

symbOr :: Symb;             pzSymbOr :: PzVal f
symbOr = symb "or";         pzSymbOr = PzSymb symbOr

symbAnd :: Symb;            pzSymbAnd :: PzVal f
symbAnd = symb "and";       pzSymbAnd = PzSymb symbAnd

-- lists
symbList :: Symb;           pzSymbList :: PzVal f
symbList = symb "list";     pzSymbList = PzSymb symbList

symbCons :: Symb;           pzSymbCons :: PzVal f
symbCons = symb "cons";     pzSymbCons = PzSymb symbCons

symbHead :: Symb;           pzSymbHead :: PzVal f
symbHead = symb "head";     pzSymbHead = PzSymb symbHead

symbTail :: Symb;           pzSymbTail :: PzVal f
symbTail = symb "tail";     pzSymbTail = PzSymb symbTail

-- dictionaries
symbDict :: Symb;           pzSymbDict :: PzVal f
symbDict = symb "dict";     pzSymbDict = PzSymb symbDict

symbKeys :: Symb;           pzSymbKeys :: PzVal f
symbKeys = symb "keys";     pzSymbKeys = PzSymb symbKeys

symbAssocs :: Symb;         pzSymbAssocs :: PzVal f
symbAssocs = symb "assocs"; pzSymbAssocs = PzSymb symbAssocs

symbContains :: Symb;           pzSymbContains :: PzVal f
symbContains = symb "contains"; pzSymbContains = PzSymb symbContains

symbGet :: Symb;           pzSymbGet :: PzVal f
symbGet = symb "get";      pzSymbGet = PzSymb symbGet

symbPut :: Symb;           pzSymbPut :: PzVal f
symbPut = symb "put";      pzSymbPut = PzSymb symbPut

symbDel :: Symb;           pzSymbDel :: PzVal f
symbDel = symb "del";      pzSymbDel = PzSymb symbDel

-- functions
symbFunc :: Symb;           pzSymbFunc :: PzVal f
symbFunc = symb "func";     pzSymbFunc = PzSymb symbFunc

symbGetImplCtx :: Symb;                 pzSymbGetImplCtx :: PzVal f
symbGetImplCtx = symb "get_impl_ctx";   pzSymbGetImplCtx = PzSymb symbGetImplCtx

symbSetImplCtx :: Symb;                 pzSymbSetImplCtx :: PzVal f
symbSetImplCtx = symb "set_impl_ctx";   pzSymbSetImplCtx = PzSymb symbSetImplCtx

symbGetExplCtx :: Symb;                 pzSymbGetExplCtx :: PzVal f
symbGetExplCtx = symb "get_expl_ctx";   pzSymbGetExplCtx = PzSymb symbGetExplCtx

symbGetArgPass :: Symb;                 pzSymbGetArgPass :: PzVal f
symbGetArgPass = symb "get_arg_pass";   pzSymbGetArgPass = PzSymb symbGetArgPass

symbGetArgs :: Symb;            pzSymbGetArgs :: PzVal f
symbGetArgs = symb "get_args";  pzSymbGetArgs = PzSymb symbGetArgs

symbGetBody :: Symb;            pzSymbGetBody :: PzVal f
symbGetBody = symb "get_body";  pzSymbGetBody = PzSymb symbGetBody

-- miscellaneous
symbCtx :: Symb
symbCtx = symb "ctx"

symbArgs :: Symb
symbArgs = symb "args"

symbX :: Symb
symbX = symb "x"

symbY :: Symb
symbY = symb "y"

symbZ :: Symb
symbZ = symb "z"

-- All the following symbols correspond to non-quoted identifiers
-- i.e. symbols that were just parsed
-- They also correspond to one-or-more-quoted symbols
symbEval :: Symb
symbEval = quoteSymb $ symb "eval"

symbQuote :: Symb
symbQuote = quoteSymb $ symb "quote"

symbUnquote :: Symb
symbUnquote = quoteSymb $ symb "unquote"

symbDeepQuote :: Symb
symbDeepQuote = quoteSymb $ symb "deep_quote"

symbDeepUnquote :: Symb
symbDeepUnquote = quoteSymb $ symb "deep_unquote"