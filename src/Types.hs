module Types where

import qualified Data.Map as M

import Data.AstExpr ( AstExpr )
import Data.FuncArgs ( FuncArgs )
import Data.FuncBody ( FuncBody )
import Data.FuncImpureArgs ( FuncImpureArgs )
import Data.Ident ( Ident )
import Data.Numb ( Numb )
import Data.Str ( Str )
import Data.Symb ( Symb )
import Data.WithPos ( Pos, WithPos )

-- Value types
data PzVal
    = PzUnit
    | PzNum Numb
    | PzStr Str
    | PzSymb Symb
    | PzList [WithPos PzVal]
    | PzDict Dict
    | PzFunc Func
    deriving (Show, Eq, Ord)

type Dict = M.Map (WithPos PzVal) (WithPos PzVal)
data Func
    = Func { implCtx :: Dict, impArgs :: FuncImpureArgs, args :: FuncArgs, body :: FuncBody }
    deriving (Show, Eq, Ord)

-- Evaluator types
type Result = Maybe (WithPos PzVal)
data Acc
    = Acc Result [StackFrame]
    deriving (Show, Eq)

data StackFrame
    = Block Dict [WithPos AstExpr]
    | Form Dict Pos (Maybe (WithPos Ident)) [WithPos AstExpr]
    | Invoc Dict Pos (Maybe (WithPos Ident)) Func [WithPos PzVal] (Maybe [WithPos AstExpr])
    deriving (Show, Eq)