module Data.Args where

import Data.Symb

data Args
    = Variadic Symb
    | Arity [Symb]
    deriving (Show, Eq, Ord)

varargs :: Args
varargs = Variadic symbArgs
