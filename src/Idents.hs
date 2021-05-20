module Idents where

import Data.Ident ( Ident(..) )

-- numbers
-- TODO

-- strings
-- TODO

-- symbols
-- TODO

-- booleans
identFalse :: Ident
identFalse = Ident "false"

identTrue :: Ident
identTrue = Ident "true"

identNot :: Ident
identNot = Ident "not"

identOr :: Ident
identOr = Ident "or"

identAnd :: Ident
identAnd = Ident "and"

-- lists
identList :: Ident
identList = Ident "list"

-- dictionaries
identDict :: Ident
identDict = Ident "dict"

-- functions
identFunc :: Ident
identFunc = Ident "func"

-- miscellaneous
identCtx :: Ident
identCtx = Ident "ctx"

identArgs :: Ident
identArgs = Ident "args"

identX :: Ident
identX = Ident "x"

identY :: Ident
identY = Ident "y"