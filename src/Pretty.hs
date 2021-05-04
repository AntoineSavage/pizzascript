module Pretty where

import Types

class Pretty a where pretty :: Int -> a -> String

prettyFrame :: Int -> StackFrame -> String
prettyFrame = undefined