module Data.Boolish ( Boolish(..), boolish ) where

import qualified Data.Map as M

import BuiltIns.Values ( pzFalse, pzTrue )
import Data.Numb ( Numb(..) )
import Data.Str ( Str(..) )
import Data.PzVal ( PzVal(..) )

data Boolish
    = FalseReal
    | Falsish
    | Truish
    | TrueReal
    deriving (Show, Eq)

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