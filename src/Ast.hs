module Ast (Ast(..), doc, parser, unparse) where

import qualified Ast.AstExpr as E
import qualified Ast.AstList as L

import Control.Monad ( liftM2, void )
import Data.Char (isControl)
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String (Parser)

data Ast
    = Ast String [E.AstExpr]
    deriving (Show, Eq)

parser :: Parser Ast
parser = uncurry (flip Ast) <$> L.parseElems doc (E.parser doc) eof

unparse :: Ast -> String
unparse (Ast d xs) = L.unparseElems d E.unparse xs

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
