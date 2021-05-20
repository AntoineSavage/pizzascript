module Data.AstExpr ( AstExpr(..), parseExpr, unparseExpr ) where

import Control.Monad
import Data.Ident ( Ident, parseIdent, unparseIdent )
import Data.Lst ( Lst, parseLst, unparseLst )
import Data.Numb ( Numb, parseNumb, unparseNumb )
import Data.Str ( Str, parseStr, unparseStr )
import Data.Symb ( Symb, parseSymb, unparseSymb )
import Data.WithPos ( WithPos(..) )
import Text.Parsec ( (<?>), (<|>), getPosition )
import Text.Parsec.String ( Parser )

data AstExpr
    = AstNum Numb
    | AstStr Str
    | AstIdent Ident
    | AstSymb Symb
    | AstList (Lst (WithPos AstExpr))
    deriving (Show, Eq, Ord)

parseExpr :: Parser () -> Parser (WithPos AstExpr) -> Parser (WithPos AstExpr)
parseExpr ign p = liftM2 WithPos getPosition $
            AstNum <$> (parseNumb <?> "number")
        <|> AstStr <$> (parseStr <?> "string")
        <|> AstIdent <$> (parseIdent <?> "identifier")
        <|> AstSymb <$> (parseSymb <?> "symbol")
        <|> AstList <$> (parseLst ign p <?> "list (or dictionary or form)")

unparseExpr :: (Maybe (WithPos AstExpr) -> String) -> WithPos AstExpr -> String
unparseExpr f e =
    case val e of
        AstNum n -> unparseNumb n
        AstStr s -> unparseStr s
        AstIdent i -> unparseIdent i
        AstSymb s -> unparseSymb s
        AstList l -> unparseLst f l