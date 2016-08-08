module Lisp.Index ( Index
                  , empty
                  , lookup
                  , insert
                  ) where

import qualified Data.IntMap.Strict as I
import Prelude hiding (id, lookup)

newtype Index a = Index (I.IntMap a, [Int]) deriving (Show)

empty :: Index a
empty = Index (I.empty, [minBound .. maxBound])

lookup :: Int -> Index a -> Maybe a
lookup id (Index (intMap, _)) = I.lookup id intMap

insert :: a -> Index a -> Maybe (Int, Index a)
insert _ (Index (_, [])) = Nothing
insert val (Index (intMap, x : xs)) = Just (x, Index (I.insert x val intMap, xs))
