module Types where

import qualified Data.Map as M

import Data.Ident ( Ident )
import Data.Symb ( Symb )
import Text.Parsec ( SourcePos )

-- Shared types
type Pos = SourcePos
data WithPos a
    = WithPos { pos :: Pos, val :: a }

-- Ignore position in show, eq and ord
instance Show a => Show (WithPos a) where show (WithPos _ x) = show x
instance Eq a => Eq (WithPos a) where (==) (WithPos _ x) (WithPos _ y) = x == y
instance Ord a => Ord (WithPos a) where compare (WithPos _ x) (WithPos _ y) = compare x y
instance Functor WithPos where fmap f (WithPos p x) = WithPos p $ f x

-- AST types
data AstExpr
    = AstNum Double
    | AstStr String
    | AstIdent Ident
    | AstSymb Symb
    | AstList AstListKind [WithPos AstExpr]
    deriving (Show, Eq, Ord)

data AstListKind
    = KindList
    | KindDict
    | KindForm
    deriving (Show, Eq, Ord)

-- Value types
data PzVal
    = PzUnit
    | PzNum Double
    | PzStr String
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