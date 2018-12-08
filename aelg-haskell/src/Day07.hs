module Day07 ( solve ) where

import           Control.Arrow
import           Data.List
import           Data.Char
import           Text.ParserCombinators.ReadP
import qualified Parsing as P

parseEdge = do
  string "Step "
  before <- get
  string " must be finished before step "
  after <- get
  string " can begin."
  eof
  return (before, after)
  

parse = map (P.run parseEdge)

solve1 g = show . reverse $ go unsorted [] g
  where 
    unsorted = nub . foldMap (\(b,a) -> [b,a]) $ g
    go :: String -> String -> [(Char, Char)] -> String
    go [] sorted g = sorted
    go unsorted sorted g = go nextUnsorted nextSorted nextG
      where
        allAfter = map snd g
        ready = filter (not . (`elem` allAfter)) unsorted
        next = minimum ready
        nextG = filter ((/=) next . fst) g
        nextUnsorted = filter (/= next) unsorted
        nextSorted = next : sorted

solve2 g = show $ go 0 [] unsorted [] g
  where 
    unsorted = nub . foldMap (\(b,a) -> [b,a]) $ g
    go time [] [] sorted g = time
    go time workers unsorted sorted g = go nextTime nextWorkers nextUnsorted nextSorted nextG
      where
        allAfter = map snd g
        ready = sort $ filter (not . (`elem` allAfter)) unsorted
        nextG = filter ((/=) nextDone . fst) g
        nextSorted = nextDone : sorted
        nextUnsorted = filter (not . (`elem` nextDone : map fst nextWorkers)) unsorted
        (nextTime, nextDone, nextWorkers) = work time workers ready
        work time workers ready
          | null ready || length workers == 5 = (nextTime, nextDone, tail workers)
          | otherwise = work time (newWorkers) (tail ready)
          where
            (nextDone, nextTime) = head workers
            newWorker = (head ready, time + 61 + (ord (head ready) - ord 'A'))
            newWorkers = sortOn snd (newWorker:workers)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
