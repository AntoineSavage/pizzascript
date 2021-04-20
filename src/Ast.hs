module Ast (ignore, parser, unparse) where

import qualified Ast.AstExpr as AstExpr

import Control.Monad ( void )
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String (Parser)

parser :: Parser () -> Parser [AstExpr.AstExpr]
parser ignore = many (AstExpr.parser ignore) >>= flip (<$) eof

unparse :: String -> [AstExpr.AstExpr] -> String
unparse sep = intercalate sep . map (AstExpr.unparse sep)

ignore :: Parser ()
ignore = void $ many $ comment <|> void space

comment :: Parser ()
comment = char '#' >> void (manyTill (noneOf []) $ void endOfLine <|> eof)