module Day14 ( solve ) where

import           Control.Arrow
import           Data.List
import qualified Data.Sequence as S
import qualified Data.ByteString.Char8 as B
import           Data.Foldable
import           Data.Char

parse :: [String] -> Int
parse = read . head

addRecipe a b xs = (anew, bnew, new)
  where
    ar = score a
    br = score b
    score c = ord (S.index xs (S.length xs - c - 1)) - ord '0'
    new = S.fromList (reverse $ show (ar + br)) S.>< xs
    anew = (a + ar + 1) `mod` S.length new
    bnew = (b + br + 1) `mod` S.length new

addWhile :: (S.Seq Char -> Bool) -> S.Seq Char -> S.Seq Char
addWhile f = go 0 1
  where
    go a b xs
      | f n = go an bn n
      | otherwise = n
      where
        (an, bn, n) = addRecipe a b xs

run i = S.reverse . addWhile ((< i + 10) . S.length) . S.reverse

solve1 i = toList . S.take 10 . S.drop a $ run a (S.fromList "37")
  where
    a = i

doubleCheck l i x
  | foundString = B.length ans
  | otherwise = doubleCheck (l*2) i x
  where
    recipes = toList (run l x)
    foundString = show i `isInfixOf` recipes
    (ans, _) = B.breakSubstring (B.pack (show i)) (B.pack recipes)

solve2 i = show $ doubleCheck 2 i (S.fromList "37")

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
