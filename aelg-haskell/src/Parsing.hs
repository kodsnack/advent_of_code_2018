module Parsing 
  ( integer
  , integerAnd
  , run 
  ) where 

import           Text.ParserCombinators.ReadP
import           Data.Char

integer :: ReadP Int
integer = skipSpaces >> read <$> ((++) <$> option "" (string "-") <*> many1 (satisfy isDigit))

integerAnd :: ReadP a -> ReadP Int
integerAnd r = integer >>= \n -> r >> return n

run :: ReadP a -> String -> a
run parser s = fst . head $ readP_to_S parser s
