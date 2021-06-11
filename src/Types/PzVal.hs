module Types.PzVal where

import qualified Data.Map as M

import Types.Func ( Func )
import Types.Numb ( Numb )
import Types.Str ( Str )
import Types.Symb ( Symb )

data Quoted;
data Evaled;

type Dict = M.Map DictKey (PzVal Evaled)

-- Ignore PzFunc impl ctx when in dict key
newtype DictKey = DictKey (PzVal Evaled)
instance Show DictKey where
    show (DictKey (PzFunc _ f)) = "PzFunc <implCtx> (" ++ show f ++ ")"
    show (DictKey v)            = show v

instance Eq DictKey where
    DictKey (PzFunc _ x) == DictKey (PzFunc _ y) = x == y
    DictKey x            == DictKey y            = x == y

instance Ord DictKey where
    DictKey (PzFunc _ x) <= DictKey (PzFunc _ y) = x <= y
    DictKey x            <= DictKey y            = x <= y

data PzVal f
    = PzUnit
    | PzNum Numb
    | PzStr Str
    | PzSymb Symb
    | PzList [PzVal f]
    | PzDict Dict
    | PzFunc Dict (Func (PzVal Quoted))
    deriving (Show, Eq)

instance Ord (PzVal a) where
    PzUnit <= _ = True

    PzNum _ <= PzUnit  = False
    PzNum x <= PzNum y = x <= y
    PzNum _ <= _       = True

    PzStr _ <= PzUnit  = False
    PzStr _ <= PzNum _ = False
    PzStr x <= PzStr y = x <= y
    PzStr _ <= _       = True

    PzSymb _ <= PzUnit   = False
    PzSymb _ <= PzNum _  = False
    PzSymb _ <= PzStr _  = False
    PzSymb x <= PzSymb y = x <= y
    PzSymb _ <= _        = True

    PzList _ <= PzUnit   = False
    PzList _ <= PzNum _  = False
    PzList _ <= PzStr _  = False
    PzList _ <= PzSymb _ = False
    PzList x <= PzList y = x <= y
    PzList _ <= _        = True

    PzDict _ <= PzUnit   = False
    PzDict _ <= PzNum _  = False
    PzDict _ <= PzStr _  = False
    PzDict _ <= PzSymb _ = False
    PzDict _ <= PzList _ = False
    PzDict x <= PzDict y = x <= y
    PzDict _ <= _        = True

    PzFunc _  _  <= PzUnit       = False
    PzFunc _  _  <= PzNum _      = False
    PzFunc _  _  <= PzStr _      = False
    PzFunc _  _  <= PzSymb _     = False
    PzFunc _  _  <= PzList _     = False
    PzFunc _  _  <= PzDict _     = False
    PzFunc dx fx <= PzFunc dy fy = case (dx < dy, dx > dy, fx < fy, fx > fy) of
        (True, _, _, _) -> True
        (_, True, _, _) -> False
        (_, _, True, _) -> True
        (_, _, _, True) -> False
        _               -> True
