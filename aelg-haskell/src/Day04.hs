module Day04 ( solve ) where

import           Control.Arrow
import           Data.List
import           Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe
import           Text.ParserCombinators.ReadP
import qualified Parsing as P

data InputLine = InputLine Timestamp Action deriving (Show, Ord, Eq)

data Timestamp = Timestamp 
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  } deriving (Show, Ord, Eq)

data Action = GuardShift Int | Wakes | Sleeps deriving (Show, Ord, Eq)

parseLine = InputLine <$> parseTimeStamp <*> parseAction

parseTimeStamp = do
  char '['
  year <- P.integer
  char '-'
  month <- P.integer
  char '-'
  day <- P.integer
  char ' '
  hour <- P.integer
  char ':'
  minute <- P.integer
  string "] "
  return $ Timestamp year month day hour minute

parseAction = parseGuard <++ parseWakes <++ parseSleeps

parseGuard = do
  string "Guard #"
  guard <- P.integer
  string " begins shift"
  eof
  return $ GuardShift guard

parseWakes = string "wakes up" >> eof >> return Wakes

parseSleeps = string "falls asleep" >> eof >> return Sleeps

parse = map (P.run parseLine)

igGuard ((InputLine _ (GuardShift x)):_) = x

ilMinute (InputLine ts _) = minute ts

groupGuard :: [Shift] -> [[Shift]]
groupGuard = groupBy g . sortOn igGuard
  where g x y = igGuard x == igGuard y

type Shift = [InputLine]

groupShifts :: [InputLine] -> [Shift]
groupShifts xs = lines
  where
    ([], lines) = foldr go ([],[]) xs
    go l@(InputLine ts (GuardShift _ )) (curr, xs) = ([], (l:curr) : xs)
    go l (curr, xs) = (l:curr, xs)

type SleepPerMinute = M.Map Int Int

minutesAsleep :: [[InputLine]] -> (Int, SleepPerMinute)
minutesAsleep xs = (igGuard . head $ xs, foldl folder M.empty xs)
  where
    folder mIn ils = mOut
      where
        (mOut, _, _) = foldl go (mIn, False, tail (ils ++ [dummy])) [1..60]
    dummy = InputLine (Timestamp 0 0 0 0 60) Sleeps
    go (m, sleeping, ts:ils) minute
      | ilMinute ts == minute = (upd m, not sleeping, ils)
      | otherwise = (upd m, sleeping, ts:ils)
        where upd m = M.alter ( Just . maybe 0 (+ if sleeping then 1 else 0)) (minute - 1) m

sumAsleep (_, m) = sum m

guardTimeMax (guard, m) = guard * maxMinute
  where
    (maxMinute, _) = M.foldlWithKey findMax (0,0) m
    findMax (maxk, max) k v
      | v > max = (k, v)
      | otherwise = (maxk, max)

solver sorter = show . guardTimeMax . head . reverse . sortOn sorter . map minutesAsleep . groupGuard . groupShifts . sort

solve1 = solver sumAsleep

maxAsleep (guard, m) = maximum m

solve2 = solver maxAsleep

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
