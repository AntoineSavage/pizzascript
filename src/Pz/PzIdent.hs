module Pz.PzIdent (PzIdent(..), PzIdentPart(..), fromAst, fromAstIdent, toAst, toAstIdent) where

import Ast.AstIdent (AstIdent(..), AstIdentPart(..))

data PzIdent
    = PzIdent PzIdentPart [PzIdentPart]
    deriving (Show, Eq)

data PzIdentPart
    = PzIdentPart Char String
    deriving (Show, Eq)

fromAst :: AstIdent -> PzIdent
fromAst (AstIdent p ps) = PzIdent (fromAstIdent p) $ map fromAstIdent ps

toAst :: PzIdent -> AstIdent
toAst (PzIdent p ps) = AstIdent (toAstIdent p) $ map toAstIdent ps

fromAstIdent :: AstIdentPart -> PzIdentPart
fromAstIdent (AstIdentPart c s) = PzIdentPart c s

toAstIdent :: PzIdentPart -> AstIdentPart
toAstIdent (PzIdentPart c s) = AstIdentPart c s