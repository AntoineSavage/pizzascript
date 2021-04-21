module Ast (Ast(..), ignore, parser, unparse) where

import qualified Ast.AstExpr as AstExpr

import Control.Monad ( void )
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String (Parser)

newtype Ast
    = Ast [AstExpr.AstExpr]
    deriving (Show, Eq)

parser :: Parser () -> Parser Ast
parser ignore = do
    es <- many (AstExpr.parser ignore)
    ignore
    eof
    return $ Ast es

unparse :: String -> Ast -> String
unparse sep (Ast es) = intercalate sep $ map (AstExpr.unparse sep) es

ignore :: Parser ()
ignore = void $ many $ comment <|> void space

comment :: Parser ()
comment = char '#' >> void (manyTill (noneOf []) $ void endOfLine <|> eof)