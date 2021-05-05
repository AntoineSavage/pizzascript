{-# LANGUAGE LambdaCase #-}
module Ast where

import Control.Monad ( liftM2, void )
import Data.Char ( ord, isControl, isPrint )
import Data.List ( intercalate )
import Data.Nat ( len, unlen, Nat(..) )
import Numeric ( readHex, showHex )
import Text.Parsec
import Text.Parsec.String ( Parser )
import Types ( AstExpr(..), AstListKind(..), Ident(..), Symb(..), WithPos(..) )

-- Ignore
ignore :: Parser () 
ignore = void $ many (comment <|> many1 space <|> many1 (satisfy isControl))

comment :: Parser String
comment = char '#' >>= fmap reverse . go . (:[]) where
    go :: String -> Parser String
    go acc = do
        meof <- optionMaybe eof
        mnl <- optionMaybe newline
        case (meof, mnl) of
            (Just _, _) -> return acc
            (_, Just nl) -> return $ nl : acc
            (_, _) -> anyChar >>= (go . (:acc))

-- Numbers
parseNum :: Parser Double
parseNum = do
    let uint = many1 digit
        sint = liftM2 (++) (option "" $ string "-") uint
        consOrEmpty h t = option "" $ liftM2 (:) h t
        dec = consOrEmpty (char '.') uint
        exp = consOrEmpty (oneOf "eE") sint
    read . concat <$> sequence [sint, dec, exp]

unparseNum :: Double -> String
unparseNum d =
    let truncated = truncate d
    in if d == fromIntegral truncated
        then show truncated
        else show d

-- Strings
parseStr :: Parser String
parseStr = char '"' >> manyTill parseChar (char '"')

unparseStr :: String -> String
unparseStr s = concat $ ["\""] ++ map unparseChar s ++ ["\""]

parseChar :: Parser Char
parseChar = do
    c <- anyChar
    if c /= '\\'
        then if isPrint c
            then return c
            else parserFail $ "Unprintable characters must be escaped: " ++ unparseChar c
        else do
            escaped <- anyChar
            case escaped of
                '"' -> return '"'
                '\\' -> return '\\'
                '/' -> return '/'
                'b' -> return '\b'
                'f' -> return '\f'
                'n' -> return '\n'
                'r' -> return '\r'
                't' -> return '\t'
                'u' -> read . ("'\\x"++) . (++"'") <$> between (char '{') (char '}' ) (parseHexCodepoint $ many1 hexDigit)
                _ -> parserFail $ "Unsupported escape sequence: " ++ ['\\', escaped]

parseHexCodepoint :: Parser String -> Parser String
parseHexCodepoint p = do
    s <- p
    let [(i, "")] = readHex s
    if i <= 0x10FFFF then return s else
        parserFail $ "Hex codepoint out of range: " ++ s

unparseChar :: Char -> String
unparseChar c =
    case c of
        '\"' -> "\\\""
        '\\' -> "\\\\"
        '/' -> "\\/"
        '\b' -> "\\b"
        '\f' -> "\\f"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ -> if isPrint c
            then [c]
            else let digits = showHex (ord c) ""
                 in "\\u{" ++ digits ++ "}"

-- Identifiers
parseIdent :: Parser Ident
parseIdent = Ident <$> liftM2 (:) parseIdentPart (many $ char '.' >> parseIdentPart)

unparseIdent :: Ident -> String
unparseIdent (Ident ps) = intercalate "." $ map unparseIdentPart ps

parseIdentPart :: Parser String
parseIdentPart = liftM2 (:)
    (letter <|> underscore)
    (many $ alphaNum  <|> underscore) where
        underscore = char '_'

unparseIdentPart :: String -> String
unparseIdentPart p = p

-- Symbols
parseSymb :: Parser Symb
parseSymb = liftM2 Symb (char '\'' >> len <$> many (char '\'')) parseIdent

unparseSymb :: Symb -> String
unparseSymb (Symb n ident) = "'" ++ unlen n '\'' ++ unparseIdent ident

-- Lists
parseList :: AstListKind -> Parser () -> Parser a -> Parser [a]
parseList k ign p =
    char (getListStart k) >>
        parseMany ign p (void $ char $ getListEnd k)

unparseList :: AstListKind ->  (Maybe a -> String) -> [a] -> String
unparseList k f es = [getListStart k] ++ unparseMany f es ++ [getListEnd k]

getListStart :: AstListKind -> Char
getListStart = \case
    KindList -> '['
    KindDict -> '{'
    KindForm -> '('

getListEnd :: AstListKind -> Char
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
            AstNum <$> (parseNum <?> "number")
        <|> AstStr <$> (parseStr <?> "string")
        <|> AstIdent <$> (parseIdent <?> "identifier")
        <|> AstSymb <$> (parseSymb <?> "symbol")
        <|> AstList KindList <$> (parseList KindList ign p <?> "list")
        <|> AstList KindDict <$> (parseList KindDict ign p <?> "dictionary")
        <|> AstList KindForm <$> (parseList KindForm ign p <?> "form")

unparseExpr :: (Maybe (WithPos AstExpr) -> String) -> WithPos AstExpr -> String
unparseExpr f e =
    case val e of
        AstNum n -> unparseNum n
        AstStr s -> unparseStr s
        AstIdent i -> unparseIdent i
        AstSymb s -> unparseSymb s
        AstList k l -> unparseList k f l