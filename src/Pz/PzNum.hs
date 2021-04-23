module Pz.PzNum (PzNum(..), fromAst, toAst) where

import Ast.AstNum (AstNum(..))

data PzNum
    = PzInteger Integer
    | PzDouble Double
    deriving (Show, Eq)

fromAst :: AstNum -> PzNum
fromAst (AstInteger n) = PzInteger n
fromAst (AstDouble d) = PzDouble d

toAst :: PzNum -> AstNum
toAst (PzInteger n) = AstInteger n
toAst (PzDouble d) = AstDouble d