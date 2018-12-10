module Day08 ( solve ) where

import           Control.Arrow
import           Control.Monad
import           Text.ParserCombinators.ReadP
import qualified Parsing as P

data Tree = Tree [Tree] [Int] deriving Show

parseMetadata = P.integerAnd $ char ' '

parseTree = do
  nChilds <- P.integerAnd $ char ' '
  nMetadata <- P.integerAnd $ char ' '
  childs <- replicateM nChilds parseTree
  metadata <- replicateM nMetadata parseMetadata
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
