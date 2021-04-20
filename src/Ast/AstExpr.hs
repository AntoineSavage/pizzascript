module Ast.AstExpr (AstExpr(..), ignore, parser, unparse) where

import qualified Ast.AstIdent as AstIdent
import qualified Ast.AstList as AstList
import qualified Ast.AstNum as AstNum
import qualified Ast.AstStr as AstStr
import qualified Ast.AstSymb as AstSymb
import Control.Monad ( void )
import Text.Parsec
import Text.Parsec.String (Parser)

data AstExpr
    = AstNum AstNum.AstNum
    | AstStr AstStr.AstStr
    | AstIdent AstIdent.AstIdent
    | AstSymb AstSymb.AstSymb
    | AstList (AstList.AstList AstExpr)
    deriving (Show, Eq)

parser :: Parser AstExpr
parser = ignore >> parseExpr
    
parseExpr :: Parser AstExpr
parseExpr = AstNum <$> AstNum.parser
        <|> AstStr <$> AstStr.parser
        <|> AstIdent <$> AstIdent.parser
        <|> AstSymb <$> AstSymb.parser
        <|> AstList <$> AstList.parser AstList.AstKindList ignore parser
        <|> AstList <$> AstList.parser AstList.AstKindDict ignore parser
        <|> AstList <$> AstList.parser AstList.AstKindStruct ignore parser
        <|> AstList <$> AstList.parser AstList.AstKindEval ignore parser

ignore :: Parser ()
ignore = void $ many $ comment <|> void space

comment :: Parser ()
comment = char '#' >> void (manyTill (noneOf []) $ void endOfLine <|> eof)

unparse :: String -> AstExpr -> String
unparse sep expr =
    case expr of
        AstNum n -> AstNum.unparse n
        AstStr s -> AstStr.unparse s
        AstIdent i -> AstIdent.unparse i
        AstSymb s -> AstSymb.unparse s
        AstList l -> AstList.unparse sep (unparse sep) l