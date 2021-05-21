module Data.Func ( Func(..), getArgPass ) where

import Data.Func.ArgPass ( ArgPass(Eval) )
import Data.Func.FuncArgs ( FuncArgs )
import Data.Func.FuncBody ( FuncBody )
import Data.Func.FuncImpureArgs ( FuncImpureArgs(..) )
import Data.WithPos ( WithPos(val) )

data Func
    = Func { impArgs :: FuncImpureArgs, args :: FuncArgs, body :: FuncBody }
    deriving (Show, Eq, Ord)

getArgPass :: Func -> ArgPass
getArgPass func = case impArgs func of
    None -> Eval
    ArgPass _ ap -> val ap
    Both _ ap _ -> val ap