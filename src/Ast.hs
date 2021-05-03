module Ast where

import BuiltIns
import Control.Monad ( liftM2, void )
import Data.Char ( ord, isControl, isPrint )
import Data.List ( intercalate )
import Data.Nat ( len, unlen, Nat(..) )
import Numeric ( readHex, showHex )
import Text.Parsec
import Text.Parsec.String ( Parser )
import Types ( Ast(..), AstExpr(..), AstListKind(..), AstVal(..), Ident(..), Symb(..) )
import Utils ( toForm, symb )

-- AST
parseAst :: Parser Ast
parseAst = uncurry (flip Ast) <$> parseMany doc (parseExpr doc) eof

unparseAst :: Ast -> String
unparseAst (Ast d es) = unparseMany d unparseExpr es

doc :: Parser String 
doc = concat <$> many (comment <|> many1 space <|> many1 (satisfy isControl))

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
parseList :: AstListKind -> Parser String -> (String -> Parser a) -> Parser ([a], String)
parseList k doc p =
    char (getListStart k) >>
        parseMany doc p (void $ char $ getListEnd k)

unparseList :: AstListKind -> String -> (a -> String) -> [a] -> String
unparseList k d f es = [getListStart k] ++ unparseMany d f es ++ [getListEnd k]

getListStart :: AstListKind -> Char
getListStart KindList = '['
getListStart KindDict = '{'
getListStart KindForm = '('

getListEnd :: AstListKind -> Char
getListEnd KindList = ']'
getListEnd KindDict = '}'
getListEnd KindForm = ')'

parseMany :: Parser String -> (String -> Parser a) -> Parser () -> Parser ([a], String)
parseMany doc elem end = do
    startDoc <- doc
    go [] startDoc
    where
        go acc d = do
            mend <- optionMaybe end
            case mend of
                Just _ -> return (reverse acc, d)
                Nothing -> do
                    e <- elem d
                    d' <- doc
                    go (e : acc) d'

unparseMany :: String -> (a -> String) -> [a] -> String
unparseMany d f es = concatMap f es ++ d

-- Expressions
parseExpr :: Parser String -> String -> Parser AstExpr
parseExpr doc d = liftM2 (`AstExpr` d) getPosition $
            AstNum <$> (parseNum <?> "number")
        <|> AstStr <$> (parseStr <?> "string")
        <|> AstIdent <$> (parseIdent <?> "identifier")
        <|> AstSymb <$> (parseSymb <?> "symbol")
        <|> uncurry (flip $ AstList KindList) <$> (parseList KindList doc (parseExpr doc) <?> "list")
        <|> uncurry (flip $ AstList KindDict) <$> (parseList KindDict doc (parseExpr doc) <?> "dictionary")
        <|> uncurry (flip $ AstList KindForm) <$> (parseList KindForm doc (parseExpr doc) <?> "form")

unparseExpr :: AstExpr -> String
unparseExpr (AstExpr _ d1 v) =
    d1 ++ case v of
        AstNum n -> unparseNum n
        AstStr s -> unparseStr s
        AstIdent i -> unparseIdent i
        AstSymb s -> unparseSymb s
        AstList k d2 l -> unparseList k d2 unparseExpr l

-- Quoting
quote :: AstExpr -> AstExpr
quote e@(AstExpr p d v) =
    let toExpr = AstExpr p d in
    case v of
        -- Numbers and strings quote as themselves
        AstNum _ -> e
        AstStr _ -> e

        -- Identifiers quote as symbols
        AstIdent ident ->
            toExpr $ AstSymb $ symb ident 

        -- Symbols quote as themselves with one more quote
        AstSymb (Symb n ident) ->
            toExpr $ AstSymb $ Symb (S n) ident

        -- Lists quote as forms prepended with list
        -- Dicts quote as forms prepended with dict
        -- Forms quote as list with elements quoted recursively
        AstList k d es ->
            toExpr $ AstList KindList d $ map quote $ toForm p k es

unquote :: AstExpr -> Either String AstExpr
unquote e@(AstExpr p d v) =
    let toExpr = AstExpr p d in
    case v of
        -- Numbers and strings unquote as themselves
        AstNum _ -> return e
        AstStr _ -> return e

        -- Identifiers cannot be unquoted
        AstIdent ident ->
            Left $ "Unquote: unexpected identifier: " ++ unparseIdent ident

        -- Symbols with one quote unquote to identifiers
        -- Symbols with two or more quotes unquote to symbols with one less quote
        AstSymb (Symb n ident) ->
            return $ toExpr $ case n of
                Z -> AstIdent ident
                (S n) -> AstSymb $ Symb n ident

        -- Lists unquote to forms with elements unquoted recursively
        -- Dicts and forms cannot be unquoted
        AstList k d es ->
            case k of
                KindList -> toExpr . AstList KindForm d <$> mapM unquote es
                KindDict -> Left $ "Unquote: unexpected dictionary: " ++ unparseList KindDict d unparseExpr es
                KindForm -> Left $ "Unquote: unexpected form: " ++ unparseList KindForm d unparseExpr es