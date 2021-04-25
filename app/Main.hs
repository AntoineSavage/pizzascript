module Main where

import qualified Ast.AstIdent as I

import Ast ( unparse, parser )
import Ast.AstSymb (AstSymb(..))
import Data.Nat ( Nat )
import Text.Parsec.String ( parseFromFile )

main :: IO ()
main = do
    mast <- parseFromFile parser "example/ast.pz"
    case mast of
        Left err -> print err
        Right ast -> writeFile "example/ast_out.pz" $ Ast.unparse ast

-- Evaluation
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