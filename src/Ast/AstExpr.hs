module Ast.AstExpr (AstExpr(..), ExprVal(..), parser, quote, unparse, unquote) where

import qualified Ast.AstIdent as I
import qualified Ast.AstList as L
import qualified Ast.AstNum as N
import qualified Ast.AstStr as St
import qualified Ast.AstSymb as Sy

import Control.Monad ( liftM2 )
import Data.Nat ( Nat(..) )
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

-- Parse / unparse expr
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
unparse (AstExpr _ d v) =
    d ++ case v of
        ValNum n -> N.unparse n
        ValStr s -> St.unparse s
        ValIdent i -> I.unparse i
        ValSymb s -> Sy.unparse s
        ValList l -> L.unparse unparse l

-- Quote / unquote expr
quote :: AstExpr -> AstExpr
quote e@(AstExpr p d v) =
    let toExpr = AstExpr p d
        identPartToExpr c s = AstExpr p "" $ ValIdent $ I.AstIdent (I.AstIdentPart c s) []
        toForm k = case k of
            L.KindList -> (identPartToExpr 'l' "ist":)
            L.KindDict -> (identPartToExpr 'd' "ict":)
            L.KindForm -> id
    in
    case v of
        -- Numbers and strings quote as themselves
        ValNum _ -> e
        ValStr _ -> e

        -- Identifiers quote as symbols
        ValIdent ident ->
            toExpr $ ValSymb $ Sy.AstSymb Z ident 

        -- Symbols quote as themselves with one more quote
        ValSymb (Sy.AstSymb n ident) ->
            toExpr $ ValSymb $ Sy.AstSymb (S n) ident

        -- Lists quote as forms prepended with list
        -- Dicts quote as forms prepended with dict
        -- Forms quote as list with elements quoted recursively
        ValList (L.AstList k d es) ->
            toExpr $ ValList $ L.AstList L.KindList d $ map quote $ toForm k es

unquote :: AstExpr -> Either String AstExpr
unquote e@(AstExpr p d v) =
    let toExpr = AstExpr p d in
    case v of
        -- Numbers and strings unquote as themselves
        ValNum _ -> return e
        ValStr _ -> return e

        -- Identifiers cannot be unquoted
        ValIdent ident ->
            Left $ "Unquote: unexpected identifier: " ++ I.unparse ident

        -- Symbols with one quote unquote to identifiers
        -- Symbols with two or more quotes unquote to symbols with one less quote
        ValSymb (Sy.AstSymb n ident) ->
            return $ toExpr $ case n of
                Z -> ValIdent ident
                (S n) -> ValSymb $ Sy.AstSymb n ident

        -- Lists unquote to forms with elements unquoted recursively
        -- Dicts and forms cannot be unquoted
        ValList l@(L.AstList k d es) ->
            case k of
                L.KindList -> toExpr . ValList . L.AstList L.KindForm d <$> mapM unquote es
                L.KindDict -> Left $ "Unquote: unexpected dictionary: " ++ L.unparse unparse l
                L.KindForm -> Left $ "Unquote: unexpected form: " ++ L.unparse unparse l