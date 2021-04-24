module Pz.PzStr (PzStr(..), eval, uneval) where

import Ast.AstStr (AstStr(..))

newtype PzStr =
    PzStr String
    deriving (Show, Eq)

eval :: AstStr -> PzStr
eval (AstStr s) = PzStr s

uneval :: PzStr -> AstStr
uneval (PzStr s) = AstStr s