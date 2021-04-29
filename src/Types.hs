module Types where

import qualified Data.Map as M

import Data.Nat ( Nat )
import Text.Parsec ( SourcePos )

newtype Ident
    = Ident [String]
    deriving (Show, Eq, Ord)

data Symb
    = Symb Nat Ident
    deriving (Show, Eq, Ord)

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
    | AstSymb Nat Ident
    | AstList ListKind AstDoc [AstExpr]
    deriving (Show, Eq, Ord)

data ListKind
    = KindList
    | KindDict
    | KindForm
    deriving (Show, Eq, Ord)

data PzArgPass
    = Eval
    | Quote
    | Unquote
    | DeepQuote
    | DeepUnquote
    deriving (Show, Eq, Ord)

data PzArgs
    = Variadic Symb
    | Arity [Symb]
    deriving (Show, Eq, Ord)

type PzDict = M.Map PzVal PzVal
type PzImpCtx = Maybe Symb

data PzVal
    = PzUnit
    | PzNum Double
    | PzStr String
    | PzSymb Symb
    | PzList [PzVal]
    | PzDict PzDict
    | PzFunc PzArgPass PzImpCtx PzArgs PzFuncBody
    deriving (Show, Eq, Ord)

data PzFuncBody
    = BuiltIn Ident
    | Custom [AstExpr]
    deriving (Show, Eq, Ord)