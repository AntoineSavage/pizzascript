module Data.Str ( Str(..), parseChar, parseHexCodepoint, parseStr, unparseChar, unparseStr ) where

import Data.Char ( ord, isPrint )
import Numeric ( readHex, showHex )
import Text.Parsec ( anyChar, char, hexDigit, between, many1, manyTill, parserFail )
import Text.Parsec.String ( Parser )

newtype Str
    = Str String
    deriving (Show, Eq, Ord)

parseStr :: Parser Str
parseStr = fmap Str $ char '"' >> manyTill parseChar (char '"')

unparseStr :: Str -> String
unparseStr (Str s) = concat $ ["\""] ++ map unparseChar s ++ ["\""]

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

parseHexCodepoint :: Parser String -> Parser String
parseHexCodepoint p = do
    s <- p
    let [(i, "")] = readHex s
    if i <= 0x10FFFF then return s else
        parserFail $ "Hex codepoint out of range: " ++ s