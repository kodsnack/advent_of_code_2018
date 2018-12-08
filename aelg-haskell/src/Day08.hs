module Day08 ( solve ) where

import           Control.Arrow
import           Text.ParserCombinators.ReadP
import qualified Parsing as P

data Tree = Tree [Tree] [Int] deriving Show

parseMetadata = do
  m <- P.integer
  char ' '
  return m

parseTree = do
  nChilds <- P.integer
  char ' '
  nMetadata <- P.integer
  char ' '
  childs <- sequence $ replicate nChilds parseTree
  metadata <- sequence $ replicate nMetadata parseMetadata
  return $ Tree childs metadata

parse = head . map (P.run parseTree . (++ " "))

sumMetadata (Tree childs metadata) = 
  sum metadata + sum (map sumMetadata childs)

treeValue (Tree [] metadata) = sum metadata
treeValue (Tree childs metadata) = sum values
  where
   valid = filter (\x -> x > 0 && x <= length childs) metadata
   values = map (treeValue . (\x -> childs !! (x-1))) valid

solve1 = show . sumMetadata

solve2 = show . treeValue

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
