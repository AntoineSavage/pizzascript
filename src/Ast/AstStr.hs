module Ast.AstStr (AstStr(..), parser, parseChar, unparse, unparseChar) where

import Data.Char as Char ( isPrint, ord )
import Control.Monad ( replicateM )
import Numeric (showHex)
import Text.Parsec
import Text.Parsec.String (Parser)

newtype AstStr =
    AstStr String
    deriving (Show, Eq)

-- Parse / unparse string
parser :: Parser AstStr
parser = AstStr <$> (char '"' >> manyTill parseChar (char '"'))

unparse :: AstStr -> String
unparse (AstStr s) = concat $ ["\""] ++ map unparseChar s ++ ["\""]

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