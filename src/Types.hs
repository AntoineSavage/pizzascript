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

-- AST types (non-evaluated)
type AstPos = SourcePos
type AstDoc = String
data Meta
    = Meta AstPos AstDoc
    deriving (Show)

-- Ignore position
instance Eq Meta where (==) (Meta _ d1) (Meta _ d2) = d1 == d2
instance Ord Meta where compare (Meta _ d1) (Meta _ d2) = compare d1 d2

data Ast
    = Ast AstDoc [AstExpr]
    deriving (Show, Eq)

data AstExpr
    = AstExpr Meta AstVal
    deriving (Show, Eq, Ord)

data AstVal
    = AstNum Double
    | AstStr String
    | AstIdent Ident
    | AstSymb Symb
    | AstList AstListKind AstDoc [AstExpr]
    deriving (Show, Eq, Ord)

data AstListKind
    = KindList
    | KindDict
    | KindForm
    deriving (Show, Eq, Ord)

-- Evaluated types
-- TODO: PzExpr with Meta
data PzVal
    = PzUnit
    | PzNum Double
    | PzStr String
    | PzSymb Symb
    | PzList [PzVal]
    | PzDict Dict
    | PzFunc Func
    deriving (Show, Eq, Ord)

type Dict = M.Map PzVal PzVal
data Func
    = Func FuncImplCtx FuncExplCtx FuncArgPass FuncArgs FuncBody
    deriving (Show, Eq, Ord)

type FuncImplCtx = Dict
type FuncExplCtx = Maybe Ident

data FuncArgPass
    = Eval
    | Quote
    | Unquote
    | DeepQuote
    | DeepUnquote
    deriving (Show, Eq, Ord)

data FuncArgs
    = ArgsVaria Ident
    | ArgsArity [Ident]
    deriving (Show, Eq, Ord)

data FuncBody
    = BodyBuiltIn Ident
    | BodyCustom [AstExpr]
    deriving (Show, Eq, Ord)