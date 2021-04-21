module Ast.AstExpr (AstExpr(..), AstVal(..), parser, unparse) where

import qualified Ast.AstIdent as AstIdent
import qualified Ast.AstList as AstList
import qualified Ast.AstNum as AstNum
import qualified Ast.AstStr as AstStr
import qualified Ast.AstSymb as AstSymb

import Text.Parsec ( (<|>), (<?>) )
import Text.Parsec.String (Parser)

data AstExpr
    = AstExpr String AstVal
    deriving (Show, Eq )

data AstVal
    = AstNum AstNum.AstNum
    | AstStr AstStr.AstStr
    | AstIdent AstIdent.AstIdent
    | AstSymb AstSymb.AstSymb
    | AstList (AstList.AstList AstExpr)
    deriving (Show, Eq)

parser :: Parser String -> Parser AstExpr
parser doc = fmap (AstExpr "") $ (doc <?> "expression header doc") >> 
        (   AstNum <$> (AstNum.parser <?> "number")
        <|> AstStr <$> (AstStr.parser <?> "string")
        <|> AstIdent <$> (AstIdent.parser <?> "identifier")
        <|> AstSymb <$> (AstSymb.parser <?> "symbol")
        <|> AstList <$> (AstList.parser AstList.AstKindList doc (parser doc) <?> "list")
        <|> AstList <$> (AstList.parser AstList.AstKindDict doc (parser doc) <?> "dictionary")
        <|> AstList <$> (AstList.parser AstList.AstKindStruct doc (parser doc) <?> "struct")
        <|> AstList <$> (AstList.parser AstList.AstKindEval doc (parser doc) <?> "evaluation")
        )

unparse :: String -> AstExpr -> String
unparse sep (AstExpr _ val) =
    case val of
        AstNum n -> AstNum.unparse n
        AstStr s -> AstStr.unparse s
        AstIdent i -> AstIdent.unparse i
        AstSymb s -> AstSymb.unparse s
        AstList l -> AstList.unparse sep (unparse sep) l