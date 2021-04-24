module Ast.AstExpr (AstExpr(..), AstVal(..), parser, unparse) where

import qualified Ast.AstIdent as AstIdent
import qualified Ast.AstList as AstList
import qualified Ast.AstNum as AstNum
import qualified Ast.AstStr as AstStr
import qualified Ast.AstSymb as AstSymb

import Control.Monad ( liftM2 )
import Text.Parsec ( SourcePos, (<|>), (<?>), getPosition )
import Text.Parsec.String ( Parser )

data AstExpr
    = AstExpr SourcePos String AstVal
    deriving (Show)

-- Ignore position in eq test
instance Eq AstExpr where 
    (==) (AstExpr _ d1 v1) (AstExpr _ d2 v2) = d1 == d2 && v1 == v2

data AstVal
    = AstValNum AstNum.AstNum
    | AstValStr AstStr.AstStr
    | AstValIdent AstIdent.AstIdent
    | AstValSymb AstSymb.AstSymb
    | AstValList (AstList.AstList AstExpr)
    deriving (Show, Eq)

parser :: Parser String -> String -> Parser AstExpr
parser doc d = liftM2 (`AstExpr` d) getPosition $
            AstValNum <$> (AstNum.parser <?> "number")
        <|> AstValStr <$> (AstStr.parser <?> "string")
        <|> AstValIdent <$> (AstIdent.parser <?> "identifier")
        <|> AstValSymb <$> (AstSymb.parser <?> "symbol")
        <|> AstValList <$> (AstList.parser AstList.AstKindList doc (parser doc) <?> "list")
        <|> AstValList <$> (AstList.parser AstList.AstKindDict doc (parser doc) <?> "dictionary")
        <|> AstValList <$> (AstList.parser AstList.AstKindForm doc (parser doc) <?> "form")

unparse :: AstExpr -> String
unparse (AstExpr _ d val) =
    d ++ case val of
        AstValNum n -> AstNum.unparse n
        AstValStr s -> AstStr.unparse s
        AstValIdent i -> AstIdent.unparse i
        AstValSymb s -> AstSymb.unparse s
        AstValList l -> AstList.unparse unparse l