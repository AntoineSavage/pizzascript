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

-- AST types
type AstPos = SourcePos
type AstDoc = String

data Ast
    = Ast AstDoc [AstExpr]
    deriving (Show, Eq)

data AstExpr
    = AstExpr AstPos AstDoc AstVal
    deriving (Show)

-- Ignore position
instance Eq AstExpr where (==) (AstExpr _ d1 v1) (AstExpr _ d2 v2) = d1 == d2 && v1 == v2
instance Ord AstExpr where
    compare (AstExpr _ d1 v1) (AstExpr _ d2 v2) =
        let dCmp = compare d1 d2
        in if dCmp /= EQ then dCmp else compare v1 v2

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

-- Value types
-- TODO: PzExpr with Meta
-- TODO: getter for Func.argPass
-- TODO: getter for Func.body
-- TODO: getter for Meta.p, meta.d
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