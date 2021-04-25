module Eval where

import qualified Ast as A
import qualified Ast.AstExpr as E
import qualified Ast.AstIdent as I
import qualified Ast.AstList as L
import qualified Ast.AstNum as N
import qualified Ast.AstStr as St
import qualified Ast.AstSymb as Sy

import Data.Nat ( Nat )

data PzVal
    = ValUnit PzUnit
    | ValNum PzNum
    | ValStr PzStr
    | ValSymb PzSymb
    | ValList PzList
    | ValDict PzDict
    | ValFunc PzFunc
    deriving (Show, Eq)

data PzUnit
    = PzUnit
    deriving (Show, Eq)

data PzNum
    = PzInteger Integer
    | PzDouble Double
    deriving (Show, Eq)

newtype PzStr =
    PzStr String
    deriving (Show, Eq)

data PzSymb
    = PzSymb Nat I.AstIdent
    deriving (Show, Eq)

data PzList
    = PzList -- TODO
    deriving (Show, Eq)

data PzDict
    = PzDict -- TODO
    deriving (Show, Eq)

data PzFunc
    = PzFunc -- TODO
    deriving (Show, Eq)