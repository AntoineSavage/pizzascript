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


argPassToSymb :: ArgPass -> Symb
argPassToSymb Eval        = symbEval
argPassToSymb Quote       = symbQuote
argPassToSymb Unquote     = symbUnquote
argPassToSymb DeepQuote   = symbDeepQuote
argPassToSymb DeepUnquote = symbDeepUnquote

symbToArgPass :: Symb -> Maybe ArgPass
symbToArgPass symb =
    let m = M.fromList
            [ (symbEval, Eval)
            , (symbQuote, Quote)
            , (symbUnquote, Unquote)
            , (symbDeepQuote, DeepQuote)
            , (symbDeepUnquote, DeepUnquote)
            ]
    in  M.lookup symb m