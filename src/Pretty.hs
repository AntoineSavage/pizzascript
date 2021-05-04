module Pretty where

import Types

class Pretty a where pretty :: Int -> a -> String

instance Pretty StackFrame where
    pretty = undefined