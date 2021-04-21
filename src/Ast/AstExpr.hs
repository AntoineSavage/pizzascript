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
    = AstValNum AstNum.AstNum
    | AstValStr AstStr.AstStr
    | AstValIdent AstIdent.AstIdent
    | AstValSymb AstSymb.AstSymb
    | AstValList (AstList.AstList AstExpr)
    deriving (Show, Eq)

parser :: Parser String -> String -> Parser AstExpr
parser doc d = fmap (AstExpr d) $
            AstValNum <$> (AstNum.parser <?> "number")
        <|> AstValStr <$> (AstStr.parser <?> "string")
        <|> AstValIdent <$> (AstIdent.parser <?> "identifier")
        <|> AstValSymb <$> (AstSymb.parser <?> "symbol")
        <|> AstValList <$> (AstList.parser AstList.AstKindList doc (parser doc) <?> "list")
        <|> AstValList <$> (AstList.parser AstList.AstKindDict doc (parser doc) <?> "dictionary")
        <|> AstValList <$> (AstList.parser AstList.AstKindStruct doc (parser doc) <?> "struct")
        <|> AstValList <$> (AstList.parser AstList.AstKindEval doc (parser doc) <?> "evaluation")

unparse :: AstExpr -> String
unparse (AstExpr d val) =
    d ++ case val of
        AstValNum n -> AstNum.unparse n
        AstValStr s -> AstStr.unparse s
        AstValIdent i -> AstIdent.unparse i
        AstValSymb s -> AstSymb.unparse s
        AstValList l -> AstList.unparse unparse l