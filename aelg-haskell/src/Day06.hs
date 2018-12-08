module Day06 ( solve ) where

import           Control.Arrow
import           Data.List
import           Data.Foldable
import           Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe
import           Data.Char
import           Text.ParserCombinators.ReadP
import qualified Parsing as P

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq, Ord)

data BoundingBox = BoundingBox Int Int Int Int deriving Show

parsePoint = do 
  x <- P.integer
  string ", "
  y <- P.integer
  eof
  return $ Point x y

parse = zip [0..] . map (P.run parsePoint)

manDist (Point x1 y1) (Point x2 y2) = 
  abs (x2 - x1) + abs (y2 - y1)

closest ps p = snd . foldl maybeMin (100000, Nothing) . map (second (manDist p)) $ ps
  where
    maybeMin (minDist, best) (other, dist)
      | dist < minDist = (dist, Just other)
      | dist == minDist = (minDist, Nothing)
      | otherwise = (minDist, best)

boundingBox ps = 
  BoundingBox (minimum . map x $ ps) 
              (minimum . map y $ ps) 
              (maximum . map x $ ps) 
              (maximum . map y $ ps)

onBoundingBox (BoundingBox left top right bottom) = map head . group . sort $
  ((Point <$> [left, right] <*> [top .. bottom]) ++
   (Point <$> [left .. right] <*> [top, bottom]))

inBoundingBox (BoundingBox left top right bottom) = map head . group . sort $
  ((Point <$> [left .. right] <*> [top .. bottom]) ++
   (Point <$> [left .. right] <*> [top .. bottom]))


-- ughh!
solve1 ps = show . snd . last . filter filterFinite .  sortOn snd . map (\x -> (head x, length x)) . group . sort $  allClosest
  where
    filterFinite (n, p) = n `elem` map fst finiteAreas
    allClosest = mapMaybe (closest ps) ib
    finiteAreas = filter (not . flip elem infiniteAreas . fst) ps
    infiniteAreas = nub . sort . mapMaybe (closest ps) $ bb
    bb = onBoundingBox . boundingBox . map snd $ ps
    ib = inBoundingBox . boundingBox . map snd $ ps

-- Note ib might be to small, but it works....
solve2 ps = show . length . filter (< 10000) . map allDistance $ ib
  where
    points = map snd ps
    allDistance p = sum . map (manDist p) $ points
    ib = inBoundingBox . boundingBox $ points

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
