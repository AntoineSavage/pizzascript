module Ops.Boolish ( boolish ) where

import qualified Data.Map as M

import Symbs ( pzSymbFalse, pzSymbTrue )
import Types.Boolish ( Boolish(..) )
import Types.Numb ( Numb(..) )
import Types.Str ( Str(..) )
import Types.PzVal ( PzVal(..) )

cases :: M.Map PzVal Boolish
cases = M.fromList
  [ (pzSymbFalse, FalseReal)
  , (pzSymbTrue, TrueReal)
  ]

boolish :: PzVal -> Boolish
boolish v = case M.lookup v cases of
  Just b -> b
  _ -> case v of
    PzUnit -> Falsish
    PzNum (Numb 0) -> Falsish
    PzStr (Str "") -> Falsish
    PzList [] -> Falsish
    PzDict d -> if M.null d then Falsish else Truish
    _ -> Truish