module Ops.Boolish ( boolish ) where

import qualified Data.Map as M

import BuiltIns.Values ( pzFalse, pzTrue )
import Types.Boolish ( Boolish(..) )
import Types.Numb ( Numb(..) )
import Types.Str ( Str(..) )
import Types.PzVal ( PzVal(..) )

boolish :: PzVal -> Boolish
boolish v
  | v == pzFalse = FalseReal
  | v == pzTrue = TrueReal
  | otherwise = case v of
    PzUnit -> Falsish
    PzNum (Numb 0) -> Falsish
    PzStr (Str "") -> Falsish
    PzList [] -> Falsish
    PzDict d -> if M.null d then Falsish else Truish
    _ -> Truish