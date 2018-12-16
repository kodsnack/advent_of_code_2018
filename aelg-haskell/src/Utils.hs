{-# LANGUAGE TupleSections #-}
module Utils
  ( genGrid
  , bfs
  ) where

import qualified Queue as Q
import qualified Data.Map.Strict as M
import           Data.Maybe

genGrid f (minX, minY, maxX, maxY) = map (map f) (line <$> [minY .. maxY])
 where
    line y = map (, y) [minX..maxX]


bfs :: Ord k => (k -> [k]) -> k -> M.Map k Int
bfs gen start = snd . head . dropWhile (not . Q.null . fst) $ iterate go (add [start] 0 Q.empty, M.empty)
  where
    add k w = Q.pushList (zip (repeat w) k)
    go (q, lengths)
      | isJust (M.lookup next lengths) = (tail, lengths)
      | otherwise = (add (gen next) (w+1) tail, M.insert next w lengths)
      where
        (Just (w, next), tail) = Q.pop q
