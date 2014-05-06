module Crdt (GCounter(..), get_gc', PNCounter(..), get_pnc') where

import Lens

-- GCounter
data GCounter = GCounter {_value :: [(String, Int)]}
    deriving(Show, Eq)

getGCounter :: GCounter -> Int
getGCounter g_counter = sum [c | (_key,c) <- (_value g_counter)]

setGCounter :: String -> Int -> GCounter -> GCounter
setGCounter c_id value g_counter = _setGCounter c_id (_value g_counter) value []
_setGCounter c_id [] value acc = GCounter {_value = acc}
_setGCounter c_id ((key,c):xs) value acc 
    | c_id == key = _setGCounter c_id xs value ((key,value):acc)
    | otherwise   = _setGCounter c_id xs value ((key,c):acc)

get_gc' c_id = (getGCounter, setGCounter c_id)

-- PN Counter
data PNCounter = PNCounter {_P :: GCounter, _N ::GCounter}

getPNCounter :: PNCounter -> Int
getPNCounter pn_counter =  ((_P pn_counter) ^. (get_gc' "any")) - ((_N pn_counter) ^. (get_gc' "any"))

setPNCounter :: String -> Int -> PNCounter -> PNCounter
setPNCounter c_id value pn_counter = pn_counter -- TODO implement setter


get_pnc' c_id = (getPNCounter, setPNCounter c_id)
