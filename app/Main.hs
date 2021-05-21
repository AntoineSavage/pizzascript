module Main where

import qualified Data.Map as M

import BuiltIns.Ctx ( builtInCtx )
import Control.Monad ( forM_, liftM2 )
import Data.AstExpr ( parseExpr )
import Data.Lst ( parseMany )
import Data.Maybe ( fromMaybe )
import Data.Nat ( Nat(..) )
import Data.StackFrame ( StackFrame(..) )
import Ignore ( ignore )
import Quote ( quote, unquote )
import Text.Parsec ( eof )
import Text.Parsec.String ( parseFromFile )
import Reduce ( Acc(..), evalFrame )

main :: IO ()
main = do
    let parser = parseMany ignore parse eof
        parse = parseExpr ignore parse
    mes <- parseFromFile parser "example/main.pz"
    case mes of
        Left err -> print err
        Right es -> go $ Acc Nothing [Block builtInCtx es]

go :: Acc -> IO ()
go (Acc result []) = putStrLn "Halting" -- no more frames: halt
go (Acc result (frame:frames)) = do
    putStrLn "=========="
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Frame: " ++ show frame
    putStrLn $ "Nbr additional frames: " ++ show (length frames)
    putStrLn "----------"
    case evalFrame result frame frames of
        Left s -> putStrLn s
        Right acc -> go acc
