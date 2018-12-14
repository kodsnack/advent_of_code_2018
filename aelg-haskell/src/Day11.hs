module Day11 ( solve ) where

import           Control.Arrow
import qualified Data.Map.Strict as M
import           Data.Tuple
import qualified Utils as U

power :: Int -> (Int, Int) -> Int
power gridSerial (x,y) = ((rackId * y + gridSerial) * rackId `div` 100) `mod` 10 - 5
  where rackId = x + 10

gen :: Int -> M.Map (Int, Int, Int) Int
gen gridSerial = foldl fill M.empty grid
  where
    grid = (,,) <$> [0..299] <*> [0..299] <*> [1..300]
    fill m (x,y,1) = M.insert (x,y,1) (power gridSerial (x,y)) m
    fill m (x,y,s)
      | x + s < 299 && y + s < 299 = M.insert (x,y,s) v m
      | otherwise = m
      where
        xs = sum . map (\x -> power gridSerial (x, y+s-1)) $ [x..x+s-2]
        ys = sum . map (\y -> power gridSerial (x+s-1, y)) $ [y..y+s-2]
        v = (m M.! (x,y, s-1)) + xs + ys + power gridSerial (x+s-1, y+s-1)

parse = gen . read . head

solve1 = showCoord . snd . maximum . map swap . M.toList . M.filterWithKey size3
  where
    showCoord (x,y,_) = show x ++ "," ++ show y
    size3 (_,_,s) _ = s == 3

solve2 = showCoord . snd . maximum . map swap . M.toList
  where
    showCoord (x,y,s) = show x ++ "," ++ show y ++ "," ++ show s

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
