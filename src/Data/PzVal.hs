module Data.PzVal ( PzVal(..), Dict ) where

import qualified Data.Map as M

import Data.AstExpr ( AstExpr )
import Data.Func ( Func )
import Data.Func.FuncArgs ( FuncArgs )
import Data.Func.FuncBody ( FuncBody )
import Data.Func.FuncImpureArgs ( FuncImpureArgs )
import Data.Ident ( Ident )
import Data.Numb ( Numb )
import Data.Str ( Str )
import Data.Symb ( Symb )

data PzVal
    = PzUnit
    | PzNum Numb
    | PzStr Str
    | PzSymb Symb
    | PzList [PzVal]
    | PzDict Dict
    | PzFunc Dict Func
    deriving (Show, Eq, Ord)

type Dict = M.Map PzVal PzVal