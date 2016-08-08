module Lisp.SymbolTable ( SymbolTable
                        , empty
                        , idToSym
                        , symToID
                        ) where

import Prelude hiding (id)
import qualified Data.Map as M

import qualified Lisp.Index as I

newtype SymbolTable a = SymbolTable (I.Index a, M.Map a Int) deriving (Show)

empty :: SymbolTable a
empty = SymbolTable (I.empty, M.empty)

idToSym :: Int -> SymbolTable a -> Maybe a
idToSym id (SymbolTable (index, _)) = I.lookup id index

symToID :: Ord a => a -> SymbolTable a -> Maybe (Int, SymbolTable a)
symToID sym table@(SymbolTable (_, symMap)) =
  maybe (insert sym table)
        (Just . flip (,) table)
        (M.lookup sym symMap)

insert :: Ord a => a -> SymbolTable a -> Maybe (Int, SymbolTable a)
insert val (SymbolTable (index, symMap)) =
  let go (id, index') = (id, SymbolTable (index', M.insert val id symMap))
  in  go <$> I.insert val index
