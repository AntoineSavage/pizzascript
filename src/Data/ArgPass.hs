module Data.ArgPass where

import qualified Data.Map as M
import Data.Symb

data ArgPass
    = Eval
    | Quote
    | Unquote
    | DeepQuote
    | DeepUnquote
    deriving (Show, Eq, Ord)


toSymb :: ArgPass -> Symb
toSymb Eval        = symbEval
toSymb Quote       = symbQuote
toSymb Unquote     = symbUnquote
toSymb DeepQuote   = symbDeepQuote
toSymb DeepUnquote = symbDeepUnquote

fromSymb :: Symb -> Maybe ArgPass
fromSymb symb =
    let m = M.fromList
            [ (symbEval, Eval)
            , (symbQuote, Quote)
            , (symbUnquote, Unquote)
            , (symbDeepQuote, DeepQuote)
            , (symbDeepUnquote, DeepUnquote)
            ]
    in  M.lookup symb m

symbEval :: Symb
symbEval = symb "eval"

symbQuote :: Symb
symbQuote = symb "quote"

symbUnquote :: Symb
symbUnquote = symb "unquote"

symbDeepQuote :: Symb
symbDeepQuote = symb "deep_quote"

symbDeepUnquote :: Symb
symbDeepUnquote = symb "deep_unquote"