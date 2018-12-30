module Day09 ( solve ) where

import           Control.Arrow
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import           Data.Maybe
import           Text.ParserCombinators.ReadP
import qualified Parsing as P

parseRule = do
  players <- P.integer
  string " players; last marble is worth "
  points <- P.integer
  string " points" >> eof
  return (players, points)

parse = head . map (P.run parseRule)

rotate i s = back S.>< front
  where
    (front, back) = S.splitAt (i `mod` S.length s) s

play (nPlayers, lastPoint) = go (S.singleton 0) 1 M.empty
 where
   go placed value points
     | value > lastPoint = points
     | value `rem` 23 == 0 = go newPlaced (value + 1) newPoints
     | otherwise = go (value S.<| rotate 2 placed) (value + 1) points
     where
       (win S.:<| newPlaced) = rotate (-7) placed
       curPlayer = (value-1) `mod` nPlayers
       newPoints = M.alter addWin curPlayer points
       addWin = Just . (win + value +) . fromMaybe 0

solve1 = show . maximum . play

solve2 = show . maximum . play . second (*100)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
