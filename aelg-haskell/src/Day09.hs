module Day09 ( solve ) where

import           Control.Arrow
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import           Text.ParserCombinators.ReadP
import qualified Parsing as P

parseRule = do
  players <- P.integer
  string " players; last marble is worth "
  points <- P.integer
  string " points" >> eof
  return (players, points)

parse = head . map (P.run parseRule)

play (nPlayers, lastPoint) = go (S.singleton 0) 1 M.empty
 where
   go :: S.Seq Int -> Int -> M.Map Int Int -> M.Map Int Int
   go placed value points
     | value > lastPoint = points
     | value `rem` 23 == 0 = go newPlaced (value + 1) newPoints
     | otherwise = go (value S.<| S.drop 2 placed S.>< S.take 2 placed) (value + 1) points
     where
       lastSeven = S.drop (S.length placed - 7) placed
       win = S.index lastSeven 0 + value
       frontSeven = S.take (S.length placed - 7) placed
       newPlaced = (S.drop 1 lastSeven) S.>< frontSeven
       curPlayer = (value - 1) `rem` nPlayers
       newPoints = M.alter addWin curPlayer points
       addWin = maybe (Just win) (Just . (win +))

solve1 = show . maximum . play

solve2 = show . maximum . play . second (*100)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
