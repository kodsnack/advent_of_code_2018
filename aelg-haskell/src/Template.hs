module Template ( solve ) where

import           Control.Arrow
import           Control.Monad
import           Data.List
import           Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe
import           Data.Char
import           Text.ParserCombinators.ReadP
import qualified Parsing as P

parse = id

--solve1 :: [String] -> String
solve1 = unlines

--solve2 :: [String] -> String
solve2 = solve1

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
