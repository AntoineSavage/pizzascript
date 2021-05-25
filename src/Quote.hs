module Quote (quote, unquote) where

import Data.Nat ( Nat(..) )
import Data.PzVal ( PzVal(..), unparseVal )
import Data.Symb ( Symb(Symb), quoteSymb, symb, unquoteSymb )
import Utils ( Result )

-- Values must be unevaled before calling quote and unquote

quote :: PzVal -> PzVal
quote v =
    case v of
        -- Numbers and strings quote as themselves
        PzNum _ -> v
        PzStr _ -> v

        -- Symbols quote as themselves, quoted
        PzSymb s -> PzSymb $ quoteSymb s

        -- Lists quote elements recursively
        PzList es -> PzList $ map quote es

        _ -> error $ "Value must be unevaluated before quoting: " ++ show v

unquote :: PzVal -> PzVal
unquote v =
    case v of
        -- Numbers and strings unquote as themselves
        PzNum _ -> v
        PzStr _ -> v

        -- Symbols unquote as themselves, unquoted
        PzSymb s -> PzSymb $ unquoteSymb s

        -- Lists unquote elements recursively
        PzList es -> PzList $ map unquote es

        _ -> error $ "Value must be unevaluated before unquoting: " ++ show v