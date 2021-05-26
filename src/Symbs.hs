module Symbs where

import Ops.Symb ( quoteSymb, symb )
import Types.PzVal ( PzVal(PzSymb) )
import Types.Symb ( Symb(..) )

-- All the following symbols correspond to quoted identifiers
-- i.e. identifiers that were just parsed
-- They also correspond to single-quoted symbols

-- generic
symbTypeOf :: Symb;         pzSymbTypeOf :: PzVal; 
symbTypeOf = symb "type_of"; pzSymbTypeOf = PzSymb symbTypeOf

symbEq :: Symb;             pzSymbEq :: PzVal
symbEq = symb "eq";         pzSymbEq = PzSymb symbEq

symbLt :: Symb;             pzSymbLt :: PzVal
symbLt = symb "lt";         pzSymbLt = PzSymb symbLt

-- numbers
symbNum :: Symb;            pzSymbNum :: PzVal
symbNum = symb "num";       pzSymbNum = PzSymb symbNum

symbAdd :: Symb;            pzSymbAdd :: PzVal
symbAdd = symb "add";       pzSymbAdd = PzSymb symbAdd

symbSub :: Symb;            pzSymbSub :: PzVal
symbSub = symb "sub";       pzSymbSub = PzSymb symbSub

symbMult :: Symb;           pzSymbMult :: PzVal
symbMult = symb "mult";     pzSymbMult = PzSymb symbMult

symbDiv :: Symb;            pzSymbDiv :: PzVal
symbDiv = symb "div";       pzSymbDiv = PzSymb symbDiv

symbRem :: Symb;            pzSymbRem :: PzVal
symbRem = symb "rem";       pzSymbRem = PzSymb symbRem

symbExp :: Symb;            pzSymbExp :: PzVal
symbExp = symb "exp";       pzSymbExp = PzSymb symbExp

symbLog :: Symb;            pzSymbLog :: PzVal
symbLog = symb "log";       pzSymbLog = PzSymb symbLog

symbFloor :: Symb;          pzSymbFloor :: PzVal
symbFloor = symb "floor";   pzSymbFloor = PzSymb symbFloor

symbCeil :: Symb;           pzSymbCeil :: PzVal
symbCeil = symb "ceil";     pzSymbCeil = PzSymb symbCeil

symbTrunc :: Symb;          pzSymbTrunc :: PzVal
symbTrunc = symb "trunc";   pzSymbTrunc = PzSymb symbTrunc

-- strings
symbStr :: Symb;            pzSymbStr :: PzVal
symbStr = symb "str";       pzSymbStr = PzSymb symbStr

symbSplit :: Symb;          pzSymbSplit :: PzVal
symbSplit = symb "split";   pzSymbSplit = PzSymb symbSplit

symbJoin :: Symb;           pzSymbJoin :: PzVal
symbJoin = symb "join";     pzSymbJoin = PzSymb symbJoin

-- symbols
symbSymb :: Symb;           pzSymbSymb :: PzVal
symbSymb = symb "symb";     pzSymbSymb = PzSymb symbSymb

symbNbrQuotes :: Symb;              pzSymbNbrQuotes :: PzVal
symbNbrQuotes = symb "nbr_quotes";  pzSymbNbrQuotes = PzSymb symbNbrQuotes

-- booleans
symbFalse :: Symb;          pzSymbFalse :: PzVal
symbFalse = symb "false";   pzSymbFalse = PzSymb symbFalse

symbTrue :: Symb;           pzSymbTrue :: PzVal
symbTrue = symb "true";     pzSymbTrue = PzSymb symbTrue

symbNot :: Symb;            pzSymbNot :: PzVal
symbNot = symb "not";       pzSymbNot = PzSymb symbNot

symbOr :: Symb;             pzSymbOr :: PzVal
symbOr = symb "or";         pzSymbOr = PzSymb symbOr

symbAnd :: Symb;            pzSymbAnd :: PzVal
symbAnd = symb "and";       pzSymbAnd = PzSymb symbAnd

-- lists
symbList :: Symb;           pzSymbList :: PzVal
symbList = symb "list";     pzSymbList = PzSymb symbList

symbCons :: Symb;           pzSymbCons :: PzVal
symbCons = symb "cons";     pzSymbCons = PzSymb symbCons

symbHead :: Symb;           pzSymbHead :: PzVal
symbHead = symb "head";     pzSymbHead = PzSymb symbHead

symbTail :: Symb;           pzSymbTail :: PzVal
symbTail = symb "tail";     pzSymbTail = PzSymb symbTail

-- dictionaries
symbDict :: Symb;           pzSymbDict :: PzVal
symbDict = symb "dict";     pzSymbDict = PzSymb symbDict

symbContains :: Symb;           pzSymbContains :: PzVal
symbContains = symb "contains"; pzSymbContains = PzSymb symbContains

symbKeys :: Symb;           pzSymbKeys :: PzVal
symbKeys = symb "keys";     pzSymbKeys = PzSymb symbKeys

symbGet :: Symb;           pzSymbGet :: PzVal
symbGet = symb "get";      pzSymbGet = PzSymb symbGet

symbPut :: Symb;           pzSymbPut :: PzVal
symbPut = symb "put";      pzSymbPut = PzSymb symbPut

symbDel :: Symb;           pzSymbDel :: PzVal
symbDel = symb "del";      pzSymbDel = PzSymb symbDel

-- functions
symbFunc :: Symb;           pzSymbFunc :: PzVal
symbFunc = symb "func";     pzSymbFunc = PzSymb symbFunc

symbGetImplCtx :: Symb;                 pzSymbGetImplCtx :: PzVal
symbGetImplCtx = symb "get_impl_ctx";   pzSymbGetImplCtx = PzSymb symbGetImplCtx

symbSetImplCtx :: Symb;                 pzSymbSetImplCtx :: PzVal
symbSetImplCtx = symb "set_impl_ctx";   pzSymbSetImplCtx = PzSymb symbSetImplCtx

symbGetExplCtx :: Symb;                 pzSymbGetExplCtx :: PzVal
symbGetExplCtx = symb "get_expl_ctx";   pzSymbGetExplCtx = PzSymb symbGetExplCtx

symbGetArgPass :: Symb;                 pzSymbGetArgPass :: PzVal
symbGetArgPass = symb "get_arg_pass";   pzSymbGetArgPass = PzSymb symbGetArgPass

symbGetArgs :: Symb;            pzSymbGetArgs :: PzVal
symbGetArgs = symb "get_args";  pzSymbGetArgs = PzSymb symbGetArgs

symbGetBody :: Symb;            pzSymbGetBody :: PzVal
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