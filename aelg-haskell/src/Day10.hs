module Day10 ( solve ) where

import           Control.Arrow
import           Text.ParserCombinators.ReadP
import qualified Parsing as P
import qualified Utils as U

type MovingPoint = ((Int, Int), (Int, Int))

parseLine = do
  string "position=<"
  x <- P.integer
  string ", "
  y <- P.integer
  string "> velocity=<"
  vx <- P.integer
  string ", "
  vy <-P.integer
  string ">"
  eof
  return ((x,y),(vx,vy))

parse = findSmallest . enumerateSteps . map (P.run parseLine)

step :: MovingPoint -> MovingPoint
step ((x,y),(vx,vy)) = ((x+vx, y+vy), (vx, vy))

enumerateSteps = zip [0..] . iterate (map step)

boundingBox :: [MovingPoint] -> (Int, Int, Int, Int)
boundingBox xs = 
  ( minimum . map (fst . fst) $ xs
  , minimum . map (snd . fst) $ xs
  , maximum . map (fst . fst) $ xs
  , maximum . map (snd . fst) $ xs
  )

printSky :: [MovingPoint] -> String
printSky xs = unlines . U.genGrid printStar $ boundingBox xs
  where
    stars :: [(Int, Int)]
    stars = map fst xs
    printStar x = if x `elem` stars then 'X' else ' '


findSmallest :: [(Int, [MovingPoint])] -> (Int, [MovingPoint])
findSmallest (x:y:xs)
  | bb y > bb x = x
  | otherwise = findSmallest (y:xs)
  where 
    bb (_, x) = bbSize . boundingBox $ x
    bbSize (xmin, ymin, xmax, ymax) = abs (xmax-xmin) + abs (ymax - ymin)

solve1 = (:) '\n' . printSky . snd

solve2 = show . fst

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
