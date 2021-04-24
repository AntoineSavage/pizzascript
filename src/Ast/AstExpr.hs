module Ast.AstExpr (AstExpr(..), AstVal(..), parser, unparse) where

import qualified Ast.AstIdent as I
import qualified Ast.AstList as L
import qualified Ast.AstNum as N
import qualified Ast.AstStr as St
import qualified Ast.AstSymb as Sy

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
    = AstValNum N.AstNum
    | AstValStr St.AstStr
    | AstValIdent I.AstIdent
    | AstValSymb Sy.AstSymb
    | AstValList (L.AstList AstExpr)
    deriving (Show, Eq)

parser :: Parser String -> String -> Parser AstExpr
parser doc d = liftM2 (`AstExpr` d) getPosition $
            AstValNum <$> (N.parser <?> "number")
        <|> AstValStr <$> (St.parser <?> "string")
        <|> AstValIdent <$> (I.parser <?> "identifier")
        <|> AstValSymb <$> (Sy.parser <?> "symbol")
        <|> AstValList <$> (L.parser L.KindList doc (parser doc) <?> "list")
        <|> AstValList <$> (L.parser L.KindDict doc (parser doc) <?> "dictionary")
        <|> AstValList <$> (L.parser L.KindForm doc (parser doc) <?> "form")

unparse :: AstExpr -> String
unparse (AstExpr _ d val) =
    d ++ case val of
        AstValNum n -> N.unparse n
        AstValStr s -> St.unparse s
        AstValIdent i -> I.unparse i
        AstValSymb s -> Sy.unparse s
        AstValList l -> L.unparse unparse l