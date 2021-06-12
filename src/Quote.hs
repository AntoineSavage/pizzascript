module Quote ( quote, unquote ) where

import Ops.Symb
import Types.Nat
import Types.PzVal
import Types.Symb
import Utils

quote :: PzVal Quoted -> PzVal Quoted
quote v = case v of
    -- Numbers and strings quote as themselves
    PzNum _ -> v
    PzStr _ -> v

    -- Symbols quote as themselves with one more quote
    PzSymb s -> PzSymb $ quoteSymb s

    -- Lists quote as themselves with elements quoted recursively
    PzList vs -> PzList $ map quote vs

    _ -> error $ "Can only quote numbers, strings, symbols and lists"
        ++ "\n was: " ++ show v

unquote :: PzVal Quoted -> Result (PzVal Quoted)
unquote v = case v of
    -- Numbers and strings unquote as themselves
    PzNum _ -> return v
    PzStr _ -> return v

    -- Quoted identifiers cannot be unquoted
    -- Symbols unquote to themselves with one less quote
    PzSymb s -> PzSymb <$> unquoteSymb s

    -- Lists unquote as themselves with elements unquoted recursively
    -- Dicts and forms cannot be unquoted
    PzList vs -> PzList <$> mapM unquote vs

    _ -> Left $ "Can only unquote numbers, strings, symbols and lists"
        ++ "\n was: " ++ show v