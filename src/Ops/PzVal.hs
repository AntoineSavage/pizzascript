{-# LANGUAGE LambdaCase #-}
module Ops.PzVal ( fromQuoted, parseList, parseMany, parseVal, unDictKey, unparseList, unparseMany, unparseVal ) where

import qualified Data.Map as M

import Control.Monad ( void )
import Ops.Numb ( parseNumb, unparseNumb )
import Ops.Str ( parseStr, unparseStr )
import Ops.Symb ( parseSymb, unparseSymb )
import Symbs ( pzSymbDict, pzSymbList )
import Text.Parsec ( char, choice, optionMaybe, (<?>), (<|>) )
import Text.Parsec.String ( Parser )
import Types.PzVal ( DictKey(..), Evaled, PzVal(..), Quoted )

fromQuoted :: PzVal Quoted -> PzVal Evaled
fromQuoted = \case
    PzNum n -> PzNum n
    PzStr s -> PzStr s
    PzSymb s -> PzSymb s
    PzList xs -> PzList $ map fromQuoted xs
    v -> error $ "Can only convert quoted values: " ++ show v

unDictKey :: DictKey -> PzVal Evaled
unDictKey (DictKey v) = v

parseVal :: Parser () -> Parser (PzVal Quoted) -> Parser (PzVal Quoted)
parseVal ign p =
        (PzNum <$> parseNumb <?> "number")
    <|> (PzStr <$> parseStr <?> "string")
    <|> (PzSymb <$> parseSymb <?> "symbol (or identifier)")
    <|> (PzList <$> parseList pzSymbList pzSymbDict ign p <?> "list (or dictionary or function)")

unparseVal :: (PzVal Quoted -> Bool -> String) -> PzVal Quoted -> String
unparseVal f = \case
    PzNum n -> unparseNumb n
    PzStr s -> unparseStr s
    PzSymb s -> unparseSymb s
    PzList xs -> unparseList pzSymbList pzSymbDict f xs
    v -> error $ "Can only unparse quoted values: " ++ show v

parseList :: Eq a => a -> a -> Parser () -> Parser a -> Parser [a]
parseList pl pd ign p =
    let go start end = char start >> parseMany ign p (void $ char end)
    in choice
        [ (pl:) <$> go '[' ']'
        , (pd:) <$> go '{' '}'
        , go '(' ')'
        ]

unparseList :: Eq a => a -> a -> (a -> Bool -> String) -> [a] -> String
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

unparseMany :: (a -> Bool -> String) -> [a] -> String
unparseMany f = \case
    []           -> ""
    [x]          -> f x True
    (x:xs@(y:_)) -> f x False ++ unparseMany f xs