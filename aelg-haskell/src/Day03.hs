module Day03 ( solve ) where

import           Control.Arrow
import           Data.List
import           Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe
import           Text.ParserCombinators.ReadP
import qualified Parsing as P

data Claim = Claim Int Int Int Int Int deriving Show

claim :: ReadP Claim
claim = do 
  claim <- Claim <$> (char '#' >> P.integer) 
                 <*> (string " @ " >> P.integer) 
                 <*> (char ',' >> P.integer) 
                 <*> (string ": " >> P.integer)
                 <*> (char 'x' >> P.integer)
  eof
  return claim

parse = map $ P.run claim 

-- All covered square inches of a claim
rectangle :: Claim -> [(Int,Int)]
rectangle (Claim _ left top w h) = (,) <$> [left .. left + w - 1] <*> [top .. top + h - 1] 

-- Returned map has value 0 for one claim at pos and value 1 for more than 1 claim
claimed :: [Claim] -> M.Map (Int,Int) Int
claimed = foldl updateClaimed M.empty
  where
    updateClaimed claimed claim = 
      foldl updateClaim claimed (rectangle claim)
    updateClaim claimed (x,y) = M.alter (Just . maybe 0 (const 1)) (x,y) claimed

solve1 :: [Claim] -> String
solve1 claims = show $ sum (claimed claims)
  
solve2 :: [Claim] -> String
solve2 claims = show $ foldl finder 0 claims
  where 
    claimed' = claimed claims
    finder found claim@(Claim theId left top w h)
      | overlapSum == 0 = theId
      | otherwise = found
      where 
        overlapSum = sum . catMaybes $ map (`M.lookup` claimed') (rectangle claim) 

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
