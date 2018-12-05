module Day05 ( solve ) where

import           Control.Arrow
import           Data.Char

parse = head

reduce = go []
  where
    go xs [] = xs
    go [] (x:xs) = go [x] xs
    go (y:ys) (x:xs)
      | y /= x && toUpper y == toUpper x = go ys xs
      | otherwise = go (x:y:ys) xs


solve1 = show . length . reduce

solve2 xs = show . minimum . map (go xs) $ ['a'..'z']
  where
    go :: String -> Char -> Int
    go xs filt = length . reduce . filter go2 $ xs
      where 
        go2 c = toLower c /= filt

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
