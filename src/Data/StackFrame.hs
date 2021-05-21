module Data.StackFrame ( StackFrame(..), setCtx ) where

import Data.AstExpr ( AstExpr )
import Data.Func ( Func )
import Data.Ident ( Ident )
import Data.WithPos ( Pos, WithPos )
import Data.PzVal ( Dict, PzVal )

data StackFrame
    = Block Dict [WithPos AstExpr]
    | Form Dict Pos (Maybe (WithPos Ident)) [WithPos AstExpr]
    | Invoc Dict Pos (Maybe (WithPos Ident)) Dict Func [WithPos PzVal] (Maybe [WithPos AstExpr])
    deriving (Show, Eq)

setCtx :: Dict -> [StackFrame] -> [StackFrame]
setCtx ctx frames = case frames of
    [] -> []
    (frame:fs) -> (:fs) $ case frame of
        Block _ es -> Block ctx es
        Form _ p mfi es-> Form ctx p mfi es
        Invoc _ p mfi ic f as es -> Invoc ctx p mfi ic f as es