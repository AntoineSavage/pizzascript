module Data.PzVal ( PzVal(..), Dict ) where

import qualified Data.Map as M

import Data.AstExpr ( AstExpr )
import Data.Func ( Func )
import Data.FuncArgs ( FuncArgs )
import Data.FuncBody ( FuncBody )
import Data.FuncImpureArgs ( FuncImpureArgs )
import Data.Ident ( Ident )
import Data.Numb ( Numb )
import Data.Str ( Str )
import Data.Symb ( Symb )
import Data.WithPos ( Pos, WithPos )

data PzVal
    = PzUnit
    | PzNum Numb
    | PzStr Str
    | PzSymb Symb
    | PzList [WithPos PzVal]
    | PzDict Dict
    | PzFunc Dict Func
    deriving (Show, Eq, Ord)

type Dict = M.Map (WithPos PzVal) (WithPos PzVal)