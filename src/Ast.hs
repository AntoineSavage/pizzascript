module Ast (Ast(..), doc, parser, unparse) where

import qualified Ast.AstExpr as AstExpr

import Control.Monad ( liftM2, void )
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String (Parser)

data Ast
    = Ast String [AstExpr.AstExpr]
    deriving (Show, Eq)

parser :: Parser Ast
parser = do
    es <- many (AstExpr.parser doc)
    doc <?> "module footer doc"
    eof
    return $ Ast "" es

unparse :: String -> Ast -> String
unparse sep (Ast _ es) = intercalate sep $ map (AstExpr.unparse sep) es

doc :: Parser String 
doc = concat <$> many (comment <|> many1 space)

comment :: Parser String
comment = liftM2 (:) (char '#') $
    manyTill (noneOf []) $ (:[]) <$> endOfLine <|> (eof >> return "")