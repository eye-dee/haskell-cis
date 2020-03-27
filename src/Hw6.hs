module Hw6 where

import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup _ (ListMap []) = Nothing
    lookup key (ListMap ((kk,vv):xs))
        | key == kk = Just vv
        | otherwise = lookup key (ListMap xs)
    insert key value (ListMap []) = ListMap [(key,value)]
    insert key value (ListMap (x@(k,v):xs))
        | key == k = ListMap ((key, value) : xs)
        | otherwise = ListMap (x : (getListMap (insert key value (ListMap xs))))
    delete key (ListMap []) = empty
    delete key (ListMap (x@(k,v):xs))
        | key == k = ListMap xs
        | otherwise = ListMap (x : (getListMap (delete key (ListMap xs))))
