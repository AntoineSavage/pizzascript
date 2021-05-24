module Data.Func ( Func(..), getArgPass ) where

import Data.Func.ArgPass ( ArgPass(Eval) )
import Data.Func.FuncArgs ( FuncArgs )
import Data.Func.FuncBody ( FuncBody )
import Data.Func.FuncImpureArgs ( FuncImpureArgs(..) )

data Func a
    = Func { impArgs :: FuncImpureArgs, args :: FuncArgs, body :: FuncBody a }
    deriving (Show, Eq, Ord)

getArgPass :: Func a -> ArgPass
getArgPass func = case impArgs func of
    None -> Eval
    ArgPass ap -> ap
    Both ap _ -> ap