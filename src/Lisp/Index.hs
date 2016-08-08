module Lisp.Index ( Index
                  , empty
                  , lookup
                  , size
                  , insert
                  , update
                  , delete
                  , select
                  ) where

import qualified Data.IntMap.Strict as I
import qualified Data.IntSet as S
import Prelude hiding (id, lookup)

newtype Index a = Index (I.IntMap a, [Int], Int) deriving (Show)

empty :: Index a
empty = Index (I.empty, [minBound .. maxBound], 0)

lookup :: Int -> Index a -> Maybe a
lookup id (Index (intMap, _, _)) = I.lookup id intMap

size :: Index a -> Int
size (Index (_, _, count)) = count

insert :: a -> Index a -> Maybe (Int, Index a)
insert _ (Index (_, [], _)) = Nothing
insert val (Index (intMap, (x : xs), count)) = Just (x, Index (I.insert x val intMap, xs, succ count))

update :: Int -> a -> Index a -> Maybe (Index a)
update id val (Index (intMap, xs, count))
  | I.member id intMap = Just $ Index (I.insert id val intMap, xs, count)
  | otherwise = Nothing

delete :: Int -> Index a -> Index a
delete id index@(Index (intMap, xs, count))
  | I.member id intMap = Index (I.delete id intMap, id : xs, pred count)
  | otherwise = index

select :: S.IntSet -> Index a -> Index a
select set index@(Index (intMap, _, _)) =
  S.foldr delete index $ I.keysSet intMap `S.difference` set
