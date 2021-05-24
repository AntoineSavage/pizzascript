module Data.PzVal ( PzVal(..), Dict, parseVal ) where

import qualified Data.Map as M

import Data.Func ( Func )
import Data.Numb ( Numb, parseNumb, unparseNumb )
import Data.Str ( Str, parseStr, unparseStr )
import Data.Symb ( Symb, parseSymb, unparseSymb )
import Text.Parsec ( (<?>), (<|>) )
import Text.Parsec.String ( Parser )

data PzVal
    = PzUnit
    | PzNum Numb
    | PzStr Str
    | PzSymb Symb
    | PzList [PzVal]
    | PzDict Dict
    | PzFunc Dict (Func PzVal)
    deriving (Show, Eq, Ord)

type Dict = M.Map PzVal PzVal

parseVal :: Parser () -> Parser PzVal -> Parser PzVal
parseVal ign p =
        PzNum <$> (parseNumb <?> "number")
    <|> PzStr <$> (parseStr <?> "string")
    <|> PzSymb <$> (parseSymb <?> "symbol (or identifier)")
--    <|> PzList <$> (parseLst ign p <?> "list (or dictionary or form)")

{-


unparseExpr :: (Maybe AstExpr -> String) -> AstExpr -> String
unparseExpr f e =
    case e of
        AstNum n -> unparseNumb n
        AstStr s -> unparseStr s
        AstIdent i -> unparseIdent i
        AstSymb s -> unparseSymb s
        AstList l -> unparseLst f l



import Control.Monad ( void )
import Text.Parsec ( char, choice, optionMaybe, (<?>) )
import Text.Parsec.String ( Parser )

data Lst a
    = Lst LstKind [a]
    deriving (Show, Eq, Ord)

data LstKind
    = KindList
    | KindDict
    | KindForm
    deriving (Show, Eq, Ord)

getStart :: LstKind -> Char
getStart = \case
    KindList -> '['
    KindDict -> '{'
    KindForm -> '('

getEnd :: LstKind -> Char
getEnd = \case
    KindList -> ']'
    KindDict -> '}'
    KindForm -> ')'

parseLst :: Parser () -> Parser a -> Parser (Lst a)
parseLst ign p =
    let parseList k = fmap (Lst k) $ char (getStart k) >> parseMany ign p (void $ char $ getEnd k)
    in choice
        [ parseList KindList
        , parseList KindDict
        , parseList KindForm
        ]

unparseLst :: (Maybe a -> String) -> Lst a -> String
unparseLst f (Lst k es) = [getStart k] ++ unparseMany f es ++ [getEnd k]

parseMany :: Parser () -> Parser a -> Parser () -> Parser [a]
parseMany ign elem end = go [] where
    go acc = ign >> optionMaybe end >>= \case
        Just _ -> return $ reverse acc
        Nothing -> elem >>= go . (:acc)

unparseMany :: (Maybe a -> String) -> [a] -> String
unparseMany f = \case
    []     -> f Nothing
    (x:xs) -> f (Just x) ++ unparseMany f xs

-}