module Ast (Ast(..), doc, parser, unparse) where

import qualified Ast.AstExpr as AstExpr

import Control.Monad ( void )
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String (Parser)

data Ast
    = Ast String [AstExpr.AstExpr]
    deriving (Show, Eq)

parser :: Parser Ast
parser = do
    es <- many (AstExpr.parser doc)
    doc
    eof
    return $ Ast "" es

unparse :: String -> Ast -> String
unparse sep (Ast _ es) = intercalate sep $ map (AstExpr.unparse sep) es

doc :: Parser ()
doc = void $ many $ comment <|> void space

comment :: Parser ()
comment = char '#' >> void (manyTill (noneOf []) $ void endOfLine <|> eof)