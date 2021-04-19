module Data.PzStr (PzStr(..), parser, parseChar, unparse, unparseChar) where

import Data.Char as Char ( isPrint, ord )
import Control.Monad ( replicateM )
import Numeric (showHex)
import Text.Parsec
import Text.Parsec.String (Parser)

newtype PzStr =
    PzStr String
    deriving (Show, Eq)

-- Parse / unparse string
parser :: Parser PzStr
parser = PzStr <$> (char '"' >> manyTill parseChar (char '"'))

unparse :: PzStr -> String
unparse (PzStr s) = concat $ ["\""] ++ map unparseChar s ++ ["\""]

-- Parse / unparse char
parseChar :: Parser Char
parseChar = do
    c <- anyChar
    if c /= '\\'
        then if Char.isPrint c
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
                'u' -> read . ("'\\x"++) . (++"'") <$> between (char '{') (char '}' ) (many1 hexDigit)
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
        _ -> if Char.isPrint c
            then [c]
            else let digits = showHex (ord c) ""
                 in "\\u{" ++ digits ++ "}"