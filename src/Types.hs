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

-- AST (non-evaluated) types
type AstPos = SourcePos
type AstDoc = String

data Ast
    = Ast AstDoc [AstExpr]
    deriving (Show, Eq)

data AstExpr
    = AstExpr AstPos AstDoc AstVal
    deriving (Show)

instance Eq AstExpr where 
    -- Ignore position
    (==) (AstExpr _ d1 v1) (AstExpr _ d2 v2) = d1 == d2 && v1 == v2

instance Ord AstExpr where 
    -- Ignore position
    compare (AstExpr _ d1 v1) (AstExpr _ d2 v2) =
        let dCmp = compare d1 d2
            vCmp = compare v1 v2
        in if dCmp /= EQ then dCmp else vCmp 

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
    = Func FuncDefCtx FuncImpCtx FuncArgPass FuncArgs FuncBody
    deriving (Show, Eq, Ord)

type FuncDefCtx = Dict
type FuncImpCtx = Maybe Ident

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