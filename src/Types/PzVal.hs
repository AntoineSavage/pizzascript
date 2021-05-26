module Types.PzVal where

import qualified Data.Map as M

import Types.Func ( Func )
import Types.Numb ( Numb )
import Types.Str ( Str )
import Types.Symb ( Symb )

type Dict = M.Map PzVal PzVal

data PzVal
    = PzUnit
    | PzNum Numb
    | PzStr Str
    | PzSymb Symb
    | PzList [PzVal]
    | PzDict Dict
    | PzFunc Dict (Func PzVal)
    deriving (Show, Eq, Ord)