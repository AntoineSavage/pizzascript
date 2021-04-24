module Ast.AstExpr (AstExpr(..), ExprVal(..), parser, unparse) where

import qualified Ast.AstIdent as I
import qualified Ast.AstList as L
import qualified Ast.AstNum as N
import qualified Ast.AstStr as St
import qualified Ast.AstSymb as Sy

import Control.Monad ( liftM2 )
import Text.Parsec ( SourcePos, (<|>), (<?>), getPosition )
import Text.Parsec.String ( Parser )

data AstExpr
    = AstExpr SourcePos String ExprVal
    deriving (Show)

-- Ignore position in eq test
instance Eq AstExpr where 
    (==) (AstExpr _ d1 v1) (AstExpr _ d2 v2) = d1 == d2 && v1 == v2

data ExprVal
    = ValNum N.AstNum
    | ValStr St.AstStr
    | ValIdent I.AstIdent
    | ValSymb Sy.AstSymb
    | ValList (L.AstList AstExpr)
    deriving (Show, Eq)

parser :: Parser String -> String -> Parser AstExpr
parser doc d = liftM2 (`AstExpr` d) getPosition $
            ValNum <$> (N.parser <?> "number")
        <|> ValStr <$> (St.parser <?> "string")
        <|> ValIdent <$> (I.parser <?> "identifier")
        <|> ValSymb <$> (Sy.parser <?> "symbol")
        <|> ValList <$> (L.parser L.KindList doc (parser doc) <?> "list")
        <|> ValList <$> (L.parser L.KindDict doc (parser doc) <?> "dictionary")
        <|> ValList <$> (L.parser L.KindForm doc (parser doc) <?> "form")

unparse :: AstExpr -> String
unparse (AstExpr _ d val) =
    d ++ case val of
        ValNum n -> N.unparse n
        ValStr s -> St.unparse s
        ValIdent i -> I.unparse i
        ValSymb s -> Sy.unparse s
        ValList l -> L.unparse unparse l