module Types where

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
import Data.PzVal ( Dict, PzVal )

-- Evaluator types
type Result = Maybe (WithPos PzVal)
data Acc
    = Acc Result [StackFrame]
    deriving (Show, Eq)

data StackFrame
    = Block Dict [WithPos AstExpr]
    | Form Dict Pos (Maybe (WithPos Ident)) [WithPos AstExpr]
    | Invoc Dict Pos (Maybe (WithPos Ident)) Dict Func [WithPos PzVal] (Maybe [WithPos AstExpr])
    deriving (Show, Eq)