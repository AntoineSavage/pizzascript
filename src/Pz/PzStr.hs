module Pz.PzStr (PzStr(..), fromAst, toAst) where

import Ast.AstStr (AstStr(..))

newtype PzStr =
    PzStr String
    deriving (Show, Eq)

fromAst :: AstStr -> PzStr
fromAst (AstStr s) = PzStr s

toAst :: PzStr -> AstStr
toAst (PzStr s) = AstStr s