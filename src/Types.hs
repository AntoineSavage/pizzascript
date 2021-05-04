module Types where

import qualified Data.Map as M

import Data.Nat ( Nat )
import Text.Parsec ( SourcePos )

-- Shared types
newtype Ident
    = Ident [String]
    deriving (Show, Eq, Ord)

data Symb
    = Symb Nat Ident
    deriving (Show, Eq, Ord)

type Pos = SourcePos
data WithPos a
    = WithPos { pos :: Pos, val :: a }
    deriving (Show)

-- Ignore position
instance Eq a => Eq (WithPos a) where (==) (WithPos _ x) (WithPos _ y) = x == y
instance Ord a => Ord (WithPos a) where compare (WithPos _ x) (WithPos _ y) = compare x y

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
    = Func { implCtx :: FuncImplCtx, explCtx :: FuncExplCtx, argPass :: FuncArgPass, args :: FuncArgs, body :: FuncBody }
    deriving (Show, Eq, Ord)

type FuncImplCtx = Dict
type FuncExplCtx = Maybe (WithPos Ident)
type FuncArgPass = WithPos ArgPass

data ArgPass
    = Eval
    | Quote
    | Unquote
    | DeepQuote
    | DeepUnquote
    deriving (Show, Eq, Ord)

data FuncArgs
    = ArgsVaria (WithPos Ident)
    | ArgsArity [WithPos Ident]
    deriving (Show, Eq, Ord)

data FuncBody
    = BodyBuiltIn Ident
    | BodyCustom [WithPos AstExpr]
    deriving (Show, Eq, Ord)