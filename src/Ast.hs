module Ast (Ast(..), doc, parser, unparse) where

import qualified Ast.AstExpr as AstExpr
import qualified Ast.AstList as AstList

import Control.Monad ( liftM2, void )
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String (Parser)

data Ast
    = Ast String [AstExpr.AstExpr]
    deriving (Show, Eq)

parser :: Parser Ast
parser = uncurry (flip Ast) <$> AstList.parseElems doc (AstExpr.parser doc) eof

unparse :: Ast -> String
unparse (Ast d xs) = AstList.unparseElems d AstExpr.unparse xs

doc :: Parser String 
doc = concat <$> many (comment <|> many1 space)

comment :: Parser String
comment = liftM2 (:) (char '#') $
    manyTill (noneOf []) $ (:[]) <$> endOfLine <|> (eof >> return "")