module BuiltIns.Pos where

import Data.WithPos ( Pos, WithPos(WithPos) )
import Text.Parsec.Pos ( newPos )

builtInPos :: Pos
builtInPos = newPos "<built-in>" 0 0

withPos :: a -> WithPos a
withPos = WithPos builtInPos