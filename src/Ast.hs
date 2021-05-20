module Ast ( ignore, comment ) where

import Control.Monad ( void )
import Data.Char ( isControl )
import Text.Parsec ( anyChar, char, newline, satisfy, space, eof, many1, optionMaybe, (<|>), many )
import Text.Parsec.String ( Parser )

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