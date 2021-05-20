{-# LANGUAGE LambdaCase #-}
module Data.ArgPass ( ArgPass(..), argPassToSymb, symbToArgPass ) where

import qualified Data.Map as M

import Data.Symb ( Symb )
import Symbs ( symbEval, symbQuote, symbUnquote, symbDeepQuote, symbDeepUnquote )

data ArgPass
    = Eval
    | Quote
    | Unquote
    | DeepQuote
    | DeepUnquote
    deriving (Show, Eq, Ord)

argPassToSymb :: ArgPass -> Symb
argPassToSymb = \case
    Eval -> symbEval
    Quote -> symbQuote
    Unquote -> symbUnquote
    DeepQuote -> symbDeepQuote
    DeepUnquote -> symbDeepUnquote

symbToArgPass :: Symb -> Maybe ArgPass
symbToArgPass = flip M.lookup m where
    m = M.fromList
        [ (symbEval, Eval)
        , (symbQuote, Quote)
        , (symbUnquote, Unquote)
        , (symbDeepQuote, DeepQuote)
        , (symbDeepUnquote, DeepUnquote)
        ]