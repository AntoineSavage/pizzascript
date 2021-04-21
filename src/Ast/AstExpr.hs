module Ast.AstExpr (AstExpr(..), AstVal(..), parser, unparse) where

import qualified Ast.AstIdent as AstIdent
import qualified Ast.AstList as AstList
import qualified Ast.AstNum as AstNum
import qualified Ast.AstStr as AstStr
import qualified Ast.AstSymb as AstSymb

import Text.Parsec ( (<|>) )
import Text.Parsec.String (Parser)

newtype AstExpr = AstExpr AstVal deriving (Show, Eq )

data AstVal
    = AstNum AstNum.AstNum
    | AstStr AstStr.AstStr
    | AstIdent AstIdent.AstIdent
    | AstSymb AstSymb.AstSymb
    | AstList (AstList.AstList AstExpr)
    deriving (Show, Eq)

parser :: Parser () -> Parser AstExpr
parser ignore = fmap AstExpr $ ignore >> 
        (   AstNum <$> AstNum.parser
        <|> AstStr <$> AstStr.parser
        <|> AstIdent <$> AstIdent.parser
        <|> AstSymb <$> AstSymb.parser
        <|> AstList <$> AstList.parser AstList.AstKindList ignore (parser ignore)
        <|> AstList <$> AstList.parser AstList.AstKindDict ignore (parser ignore)
        <|> AstList <$> AstList.parser AstList.AstKindStruct ignore (parser ignore)
        <|> AstList <$> AstList.parser AstList.AstKindEval ignore (parser ignore)
        )

unparse :: String -> AstExpr -> String
unparse sep (AstExpr val) =
    case val of
        AstNum n -> AstNum.unparse n
        AstStr s -> AstStr.unparse s
        AstIdent i -> AstIdent.unparse i
        AstSymb s -> AstSymb.unparse s
        AstList l -> AstList.unparse sep (unparse sep) l