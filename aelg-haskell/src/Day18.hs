module Day18 ( solve ) where

import           Control.Arrow
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Utils as U

data Spot = Tree | Open | Yard deriving (Eq, Show)

parse i = m
  where
    m = foldl go M.empty (concat $ U.genGrid id (0, 0, length i - 1, length (head i) - 1))
    go m pos@(y,x)
      | c == '.' = M.insert pos Open m
      | c == '|' = M.insert pos Tree m
      | c == '#' = M.insert pos Yard m
      where c = i !! y !! x

run :: M.Map (Int, Int) Spot -> M.Map (Int, Int) Spot
run m = M.mapWithKey go m
  where
    go k Open
      | count Tree k >= 3 = Tree
      | otherwise = Open 
    go k Tree
      | count Yard k >= 3 = Yard
      | otherwise = Tree 
    go k Yard
      | count Tree k >= 1 && count Yard k >= 1 = Yard
      | otherwise = Open
    adjacent (y,x) = mapMaybe (`M.lookup` m) [(y-1, x-1), (y-1, x), (y-1, x+1), (y, x-1), (y, x+1), (y+1, x-1), (y+1, x), (y+1, x+1)]
    count :: Spot -> (Int, Int) -> Int
    count t = length . filter (==t) . adjacent

value m = numTree m * numYard m
  where
    numTree = M.size . M.filter (== Tree)
    numYard = M.size . M.filter (== Yard)

solve1 m = show . value $ mAfter
  where
    mAfter = iterate run m !! 10

findCycle ((fIndex1, f1):(fIndex2, f2):fast) ((sIndex, s):slow)
  | f1 == s = (sIndex, fIndex1 - sIndex)
  | f2 == s = (sIndex, fIndex2 - sIndex)
  | otherwise = findCycle fast slow

solve2 m = show . value . snd $ indexed !! (cycleStart + ((1000000000 - cycleStart) `mod` cycleMultiple))
  where
    indexed = zip [0..] (iterate run m)
    (cycleStart, cycleMultiple) = findCycle (drop 1 indexed)  indexed

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
