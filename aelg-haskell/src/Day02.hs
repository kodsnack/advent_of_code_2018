module Day02
  ( solve
  ) where

import           Control.Arrow
import           Data.List
import           Data.Monoid

parse = id

solve1 xs = show $ getSum num2 * getSum num3
  where
    (num2, num3) = foldMap forEachId xs
    forEachId = has2sAnd3s . map length . group . sort
    sumOfTrue True = Sum 1
    sumOfTrue False = Sum 0
    has2sAnd3s xs = (sumOfTrue $ 2 `elem` xs, sumOfTrue $ 3 `elem` xs)

solve2 xs = snd . head . filter (uncurry oneLonger) $ saveFirstArg removeUnequal <$> xs <*> xs
  where
    oneLonger x y = length x - length y == 1
    saveFirstArg f x y = (x, f x y)
    removeUnequal xs ys = foldl f "" $ zip xs ys
      where f s (x, y)
              | x == y = s ++ [x]
              | otherwise = s

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
