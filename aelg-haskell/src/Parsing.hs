module Parsing 
  ( integer
  , run 
  ) where 

import           Text.ParserCombinators.ReadP
import           Data.Char

integer :: ReadP Int
integer = read <$> many1 (satisfy isDigit)

run :: ReadP a -> String -> a
run parser s = fst . head $ readP_to_S parser s
