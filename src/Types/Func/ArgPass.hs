module Types.Func.ArgPass where

data ArgPass
    = Eval
    | Quote
    | Unquote
    | DeepQuote
    | DeepUnquote
    deriving (Show, Eq)

-- Automatic Ord instance not picked up by coverage tool
instance Ord ArgPass where
    Quote       <= Eval      = False
    Unquote     <= Eval      = False
    DeepQuote   <= Eval      = False
    DeepUnquote <= Eval      = False

    Unquote     <= Quote     = False
    DeepQuote   <= Quote     = False
    DeepUnquote <= Quote     = False

    DeepQuote   <= Unquote   = False
    DeepUnquote <= Unquote   = False

    DeepUnquote <= DeepQuote = False

    _           <= _         = True