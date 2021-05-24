module Data.StackFrame ( StackFrame(..), setCtx ) where

import Data.AstExpr ( AstExpr )
import Data.Func ( Func )
import Data.Ident ( Ident )
import Data.PzVal ( Dict, PzVal )

data StackFrame
    = Block Dict [AstExpr]
    | Form Dict (Maybe Ident) [AstExpr]
    | Invoc Dict (Maybe Ident) Dict Func [PzVal] (Maybe [AstExpr])
    deriving (Show, Eq)

setCtx :: Dict -> [StackFrame] -> [StackFrame]
setCtx ctx frames = case frames of
    [] -> []
    (frame:fs) -> (:fs) $ case frame of
        Block _ es -> Block ctx es
        Form _ mfi es-> Form ctx mfi es
        Invoc _ mfi ic f as es -> Invoc ctx mfi ic f as es