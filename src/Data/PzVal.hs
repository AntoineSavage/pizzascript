module Data.PzVal where

import qualified Ast as A
import qualified Data.Map as M

import Data.ArgPass ( ArgPass(..) )
import Data.Args ( Args )
import Data.Ident ( Ident(..) )
import Data.Symb ( Symb(..) )

data PzVal
    = PzUnit
    | PzNum Double
    | PzStr String
    | PzSymb Symb
    | PzList [PzVal]
    | PzDict Dict
    | PzFunc ArgPass ImpureCtx Args FuncBody
    deriving (Show, Eq, Ord)

type Dict = M.Map PzVal PzVal

type ImpureCtx = Maybe Symb

data FuncBody
    = BuiltIn Ident
    | Custom [A.AstExpr]
    deriving (Show, Eq, Ord)