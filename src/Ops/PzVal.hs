{-# LANGUAGE LambdaCase #-}
module Ops.PzVal ( pd, pl, parseList, parseMany, parseVal, unparseList, unparseMany, unparseVal ) where

import qualified Data.Map as M

import Control.Monad ( void )
import Ops.Numb ( parseNumb, unparseNumb )
import Ops.Str ( parseStr, unparseStr )
import Ops.Symb ( parseSymb, unparseSymb )
import Symbs ( symbDict, symbList )
import Text.Parsec ( char, choice, optionMaybe, (<?>), (<|>) )
import Text.Parsec.String ( Parser )
import Types.Func ( Func )
import Types.Numb ( Numb  )
import Types.PzVal ( PzVal(..) )
import Types.Str ( Str  )
import Types.Symb ( Symb  )

parseVal :: Parser () -> Parser PzVal -> Parser PzVal
parseVal ign p =
        PzNum <$> (parseNumb <?> "number")
    <|> PzStr <$> (parseStr <?> "string")
    <|> PzSymb <$> (parseSymb <?> "symbol (or identifier)")
    <|> PzList <$> (parseList pl pd ign p <?> "list (or dictionary or function)")

unparseVal :: (Maybe PzVal -> String) -> PzVal -> String
unparseVal f = \case
    PzNum n -> unparseNumb n
    PzStr s -> unparseStr s
    PzSymb s -> unparseSymb s
    PzList xs -> unparseList pl pd f xs
    v -> error $ "Can only unparse quoted values: " ++ show v

pl :: PzVal
pl = PzSymb symbList

pd :: PzVal
pd = PzSymb symbDict

parseList :: Eq a => a -> a -> Parser () -> Parser a -> Parser [a]
parseList pl pd ign p =
    let go start end = char start >> parseMany ign p (void $ char end)
    in choice
        [ (pl:) <$> go '[' ']'
        , (pd:) <$> go '{' '}'
        , go '(' ')'
        ]

unparseList :: Eq a => a -> a -> (Maybe a -> String) -> [a] -> String
unparseList pl pd f elems =
    let go start end ys = [start] ++ unparseMany f ys ++ [end]
    in case elems of
        (x:xs) -> if x == pl then go '[' ']' xs
            else if x == pd then go '{' '}' xs
            else go '(' ')' elems
        [] -> go '(' ')' []

parseMany :: Parser () -> Parser a -> Parser () -> Parser [a]
parseMany ign elem end = go [] where
    go acc = ign >> optionMaybe end >>= \case
        Just _ -> return $ reverse acc
        Nothing -> elem >>= go . (:acc)

unparseMany :: (Maybe a -> String) -> [a] -> String
unparseMany f = \case
    []     -> f Nothing
    (x:xs) -> f (Just x) ++ unparseMany f xs