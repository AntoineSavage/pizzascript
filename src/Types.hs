module Types where

import qualified Data.Map as M

import Data.Ident ( Ident )
import Data.Lst ( Lst )
import Data.Numb ( Numb )
import Data.Str ( Str )
import Data.Symb ( Symb )
import Data.WithPos ( WithPos, Pos )
import Text.Parsec ( SourcePos )

-- AST types
data AstExpr
    = AstNum Numb
    | AstStr Str
    | AstIdent Ident
    | AstSymb Symb
    | AstList (Lst (WithPos AstExpr))
    deriving (Show, Eq, Ord)

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
    = Func { implCtx :: FuncImplCtx, impArgs :: FuncImpureArgs, args :: FuncArgs, body :: FuncBody }
    deriving (Show, Eq, Ord)

type FuncImplCtx = Dict
data FuncImpureArgs
    = None
    | ArgPass Pos (WithPos ArgPass)
    | Both Pos (WithPos ArgPass) (WithPos Ident)
    deriving (Show, Eq, Ord)

data ArgPass
    = Eval
    | Quote
    | Unquote
    | DeepQuote
    | DeepUnquote
    deriving (Show, Eq, Ord)

data FuncArgs
    = ArgsVaria (WithPos Ident)
    | ArgsArity Pos [WithPos Ident]
    deriving (Show, Eq, Ord)

data FuncBody
    = BodyBuiltIn Ident
    | BodyCustom [WithPos AstExpr]
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