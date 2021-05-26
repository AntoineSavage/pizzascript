{-# LANGUAGE LambdaCase #-}
module Ops.Func.ArgPass ( argPassToSymb, symbToArgPass ) where

import qualified Data.Map as M

import Symbs ( symbEval, symbQuote, symbUnquote, symbDeepQuote, symbDeepUnquote )
import Types.Func.ArgPass ( ArgPass(..) )
import Types.Symb ( Symb )

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