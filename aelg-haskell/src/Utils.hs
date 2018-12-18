{-# LANGUAGE TupleSections #-}
module Utils
  ( genGrid
  , bfs
  , parallel
  ) where

import qualified Queue as Q
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Control.Parallel.Strategies

genGrid f (minX, minY, maxX, maxY) = map (map f) (line <$> [minY .. maxY])
 where
    line y = map (, y) [minX..maxX]


bfs :: Ord k => (k -> [k]) -> [k] -> M.Map k Int
bfs gen start = go (add start 0 Q.empty) M.empty
  where
    add k w = Q.pushList (zip (repeat w) k)
    go q seen
      | Q.null q = seen
      | M.member next seen = go tail seen
      | otherwise = go (add (gen next) (w+1) tail) (M.insert next w seen)
      where
        (Just (w, next), tail) = Q.pop q


parallel :: (NFData a, Traversable f) => f a -> f a
parallel = (`using` parTraversable rdeepseq)
