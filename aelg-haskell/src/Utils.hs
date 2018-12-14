{-# LANGUAGE TupleSections #-}
module Utils
  ( genGrid
  ) where

genGrid f (minX, minY, maxX, maxY) = map (map f) (line <$> [minY .. maxY])
 where
    line y = map (, y) [minX..maxX]
