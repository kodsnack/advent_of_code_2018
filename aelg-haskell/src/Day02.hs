module Day02 ( solve ) where

import           Control.Arrow
import           Data.List
import           Data.Monoid

parse = id

solve1 xs = show $ getSum num2 * getSum num3
  where
    (num2, num3) = foldMap forEachId xs
    forEachId = has2sAnd3s . map length . group . sort
    countIf a = Sum $ if a then 1 else 0
    has2sAnd3s xs = (countIf $ 2 `elem` xs, countIf $ 3 `elem` xs)

solve2 xs = snd . head . filter (uncurry oneLonger) $ saveFirstArg removeUnequal <$> xs <*> xs
  where
    oneLonger x y = length x - length y == 1
    saveFirstArg f x y = (x, f x y)
    removeUnequal xs ys = map fst . filter (uncurry (==)) $ zip xs ys

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
