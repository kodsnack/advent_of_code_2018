module Day12 ( solve ) where

import           Control.Arrow
import           Text.ParserCombinators.ReadP
import qualified Parsing as P

parseInput = do
  string "initial state: "
  initial <- many (char '#' <++ char '.')
  string "\n\n"
  rules <- many parseRule
  eof
  return (initial, rules)

parseRule = do
  pattern <- many (char '#' <++ char '.')
  string " => "
  new <- (char '#' <++ char '.')
  char '\n'
  return (pattern, new)

parse = P.run parseInput . unlines

genPlant = map fst . filter ((== '#') . snd)

step creates initial = reverse $ go "" ("...." ++ initial ++ "....")
  where
    go s a
      | length a < 5 = s
      | take 5 a `elem` creates = go ('#' : s) (drop 1 a)
      | otherwise = go ('.' : s) (drop 1 a)

calc (initial, rules) n = sum . map fst . filter ((== '#') . snd) . zip [n*(-2) .. ] . head . drop n $ iterate (step creates) initial
  where 
    creates = genPlant rules

solve1 (initial, rules) = show . sum . map fst . filter ((== '#') . snd) . zip [-40 .. ] . (!! 20) $ iterate (step creates) initial
  where 
    creates = genPlant rules

diffs s [x] = s
diffs s (x:y:xs) = y-x : diffs s (y:xs)

-- My god
solve2 (initial, rules) = show $ (50000000000 - 400) * diffAt400 + at400
  where 
    creates = genPlant rules
    initialValues = map (calc (initial, rules)) [0..400]
    diffAt400 = last $ diffs [] initialValues
    at400 = last initialValues

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
