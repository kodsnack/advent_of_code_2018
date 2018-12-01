module Day01 ( solve ) where

import           Control.Arrow
import qualified Data.Set as S

parse :: [String] -> [Int]
parse = map ( read . filter (/= '+'))

solve1 = show . sum

solve2 = show . seenTwice 0 S.empty . cycle
  where 
    seenTwice a seen (x:xs)
      | current `S.member` seen = current
      | otherwise = seenTwice current (current `S.insert` seen) xs
      where current = a + x


solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
