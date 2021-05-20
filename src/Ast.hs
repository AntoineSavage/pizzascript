{-# LANGUAGE LambdaCase #-}
module Ast where

import Control.Monad ( liftM2, void )
import Data.AstExpr ( AstExpr(..) )
import Data.Char ( isControl )
import Data.Ident ( parseIdent, unparseIdent )
import Data.Lst ( Lst(..), LstKind(..), parseLst, unparseLst )
import Data.Numb ( parseNumb, unparseNumb )
import Data.Str ( parseStr, unparseStr )
import Data.Symb ( parseSymb, unparseSymb )
import Data.WithPos ( WithPos(WithPos, val) )
import Text.Parsec
import Text.Parsec.String ( Parser )

-- Ignore
ignore :: Parser ()
ignore = void $ many (comment <|> void (many1 space) <|> void (many1 $ satisfy isControl))

comment :: Parser ()
comment = char '#' >> go where
    go = do
        meof <- optionMaybe eof
        mnl <- optionMaybe newline
        case (meof, mnl) of
            (Just _, _) -> return ()
            (_, Just nl) -> return ()
            (_, _) -> anyChar >> go

-- Lists
parseList :: LstKind -> Parser () -> Parser a -> Parser [a]
parseList k ign p =
    char (getListStart k) >>
        parseMany ign p (void $ char $ getListEnd k)

unparseList :: LstKind ->  (Maybe a -> String) -> [a] -> String
unparseList k f es = [getListStart k] ++ unparseMany f es ++ [getListEnd k]

getListStart :: LstKind -> Char
getListStart = \case
    KindList -> '['
    KindDict -> '{'
    KindForm -> '('

getListEnd :: LstKind -> Char
getListEnd = \case
    KindList -> ']'
    KindDict -> '}'
    KindForm -> ')'

parseMany :: Parser () -> Parser a -> Parser () -> Parser [a]
parseMany ign elem end = go [] where
    go acc = ign >> optionMaybe end >>= \case
        Just _ -> return $ reverse acc
        Nothing -> elem >>= go . (:acc)

unparseMany :: (Maybe a -> String) -> [a] -> String
unparseMany f = \case
    []     -> f Nothing
    (x:xs) -> f (Just x) ++ unparseMany f xs

-- Expressions
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
        AstList (Lst k l) -> unparseList k f l