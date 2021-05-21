module Data.Boolish ( Boolish(..), boolish ) where

import qualified Data.Map as M

import Data.Numb ( Numb(Numb) )
import Data.Str ( Str(Str) )
import Data.PzVal ( PzVal(..) )
import Data.WithPos ( WithPos(val) )
import Values ( pzFalse, pzTrue )

data Boolish
    = FalseReal
    | Falsish
    | Truish
    | TrueReal
    deriving (Show, Eq)

boolish :: WithPos PzVal -> Boolish
boolish v
  | v == pzFalse = FalseReal
  | v == pzTrue = TrueReal
  | otherwise = case val v of
    PzUnit -> Falsish
    PzNum (Numb 0) -> Falsish
    PzStr (Str "") -> Falsish
    PzList [] -> Falsish
    PzDict d -> if M.null d then Falsish else Truish
    _ -> Truish