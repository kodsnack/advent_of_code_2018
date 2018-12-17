module Day17 ( solve ) where

import           Control.Arrow
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Tuple
import           Text.ParserCombinators.ReadP
import qualified Parsing as P
import qualified Utils as U

parseX = do
  string "x="
  x <- P.integerAnd (string ", y=")
  y1 <- P.integerAnd (string "..")
  y2 <- P.integerAnd eof
  return (x, y1, x, y2)


parseY = do
  string "y="
  y <- P.integerAnd (string ", x=")
  x1 <- P.integerAnd (string "..")
  x2 <- P.integerAnd eof
  return (x1, y, x2, y)


toSet = S.fromList . concat . concatMap (U.genGrid id)

valid m pos = not $ S.member pos m

flow maxY m (x,y)
  | y == maxY = []
  | not $ null down = down
  | otherwise = sideways
  where
    down = filter (valid m) [(x, y+1)]
    sideways = filter (valid m) [(x+1, y), (x-1, y)]

settled m (x,y) = ((x,y), not . any ((/= y) . snd) . M.keys $ U.bfs (flow (y+1) m) [(x,y)])

findMaxY = fst . maximum . map swap . S.toList
findMinY = fst . minimum . map swap . S.toList

run maxY m atRest
  | S.size atRest == S.size nextAtRest = (atRest, S.fromList flowing)
  | otherwise = run maxY m nextAtRest
  where
    solidAndAtRest = S.union m atRest
    flowing = M.keys $ U.bfs (flow maxY solidAndAtRest) [(500, 0)]
    newAtRest = map fst . filter snd . map (settled solidAndAtRest) $ flowing
    nextAtRest = S.union atRest . S.fromList $ newAtRest

calc m = (maxY, minY, run maxY m S.empty)
  where
    maxY = findMaxY m
    minY = findMinY m

parse = calc . toSet . map (P.run (parseX <++ parseY))

solve1 (maxY, minY, (atRest, flowing)) = show $ S.size water
  where
    water = S.filter validY $ S.union atRest flowing
    validY (_,y) = y <= maxY && y >= minY

solve2 (maxY, minY, (atRest, flowing)) = show $ S.size atRest

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
