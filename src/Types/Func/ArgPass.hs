module Types.Func.ArgPass where

data ArgPass
    = Eval
    | Quote
    | Unquote
    | DeepQuote
    | DeepUnquote
    deriving (Show, Eq, Ord)