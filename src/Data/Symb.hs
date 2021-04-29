module Data.Symb where

import Data.Ident ( Ident (Ident), ident )
import Data.Nat ( Nat(Z) )

data Symb
    = Symb Nat Ident
    deriving (Show, Eq, Ord)

fromIdent :: Ident -> Symb
fromIdent = Symb Z

symb :: String -> Symb
symb = fromIdent . ident

symbTrue :: Symb
symbTrue = symb "true"

symbFalse :: Symb
symbFalse = symb "false"

symbArgs :: Symb
symbArgs = symb "args"