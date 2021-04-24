module Pz.PzNum (PzNum(..), eval, uneval) where

import Ast.AstNum (AstNum(..))

data PzNum
    = PzInteger Integer
    | PzDouble Double
    deriving (Show, Eq)

eval :: AstNum -> PzNum
eval (AstInteger n) = PzInteger n
eval (AstDouble d) = PzDouble d

uneval :: PzNum -> AstNum
uneval (PzInteger n) = AstInteger n
uneval (PzDouble d) = AstDouble d