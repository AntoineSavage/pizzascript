module Quote (quote, unquote) where

import Data.Nat ( Nat(..) )
import Data.PzVal ( PzVal(..), unparseVal )
import Data.Symb ( Symb(Symb), symb )
import Utils ( Result )

-- Values must be unevaled before calling quote and unquote

quote :: PzVal -> PzVal
quote v =
    case v of
        -- Numbers and strings quote as themselves
        PzNum _ -> v
        PzStr _ -> v

        -- Symbols quote as themselves with one more quote
        PzSymb (Symb n f ns) -> PzSymb $ Symb (S n) f ns

        -- Lists quote elements recursively
        PzList es -> PzList $ map quote es

        _ -> error $ "Value must be unevaluated before quoting: " ++ show v

unquote :: PzVal -> PzVal
unquote v =
    case v of
        -- Numbers and strings unquote as themselves
        PzNum _ -> v
        PzStr _ -> v

        -- Single-quoted symbols (i.e. parsed identifiers) cannot be unquoted
        -- Other symbols unquote to one less quote
        PzSymb (Symb (S n) f ns) -> PzSymb $ Symb n f ns

        -- Lists unquote elements recursively
        PzList es -> PzList $ map unquote es

        _ -> error $ "Value must be unevaluated before unquoting: " ++ show v