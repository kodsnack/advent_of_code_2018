module Day13 ( solve ) where

import           Prelude hiding (Left, Right)
import           Control.Arrow
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Utils as U

parse = id

data Dir = Up | Down | Left | Right deriving Show

data Turn = TLeft | Straight | TRight deriving Show

data Cart = Cart {pos :: (Int, Int), dir :: Dir, nextTurn :: Turn} deriving Show

move '|' c@(Cart (y,x) Up _) = c {pos = (y-1, x) }
move '\\' c@(Cart (y,x) Up _) = c {pos = (y, x-1), dir = Left }
move '/' c@(Cart (y,x) Up _) = c {pos = (y, x+1), dir = Right }
move '|' c@(Cart (y,x) Down _) = c {pos = (y+1, x) }
move '\\' c@(Cart (y,x) Down _) = c {pos = (y, x+1), dir = Right }
move '/' c@(Cart (y,x) Down _) = c {pos = (y, x-1), dir = Left }
move '-' c@(Cart (y,x) Left _) = c {pos = (y, x-1) }
move '\\' c@(Cart (y,x) Left _) = c {pos = (y-1, x), dir = Up }
move '/' c@(Cart (y,x) Left _) = c {pos = (y+1, x), dir = Down }
move '-' c@(Cart (y,x) Right _) = c {pos = (y, x+1) }
move '\\' c@(Cart (y,x) Right _) = c {pos = (y+1, x), dir = Down }
move '/' c@(Cart (y,x) Right _) = c {pos = (y-1, x), dir = Up }
move '+' (Cart (y,x) Up TLeft) = Cart (y, x-1) Left Straight
move '+' (Cart (y,x) Up Straight) = Cart (y-1, x) Up TRight
move '+' (Cart (y,x) Up TRight) = Cart (y, x+1) Right TLeft
move '+' (Cart (y,x) Down TLeft) = Cart (y, x+1) Right Straight
move '+' (Cart (y,x) Down Straight) = Cart (y+1, x) Down TRight
move '+' (Cart (y,x) Down TRight) = Cart (y, x-1) Left TLeft
move '+' (Cart (y,x) Left TLeft) = Cart (y+1, x) Down Straight
move '+' (Cart (y,x) Left Straight) = Cart (y, x-1) Left TRight
move '+' (Cart (y,x) Left TRight) = Cart (y-1, x) Up TLeft
move '+' (Cart (y,x) Right TLeft) = Cart (y-1, x) Up Straight
move '+' (Cart (y,x) Right Straight) = Cart (y, x+1) Right TRight
move '+' (Cart (y,x) Right TRight) = Cart (y+1, x) Down TLeft

moveCart Nothing paths carts = (Nothing, carts)
moveCart (Just cart) paths carts
  | crash = (Just $ pos movedCart, M.delete (pos movedCart) $ M.delete (pos cart) carts)
  | otherwise = (Nothing, M.delete (pos cart) $ M.insert (pos movedCart)  movedCart carts)
  where
    crash = isJust $ M.lookup (pos movedCart) carts
    (y, x) = pos cart
    movedCart = move (paths !! y !! x) cart

runTick crashes paths carts = go keys crashes carts
  where
    keys = M.keys carts
    go [] crashes carts = (crashes, carts)
    go (x:xs) crashes carts = go xs (maybe crashes (:crashes) crash) updated
      where
        (crash, updated) = moveCart (M.lookup x carts) paths carts
    
findCrashes paths = go []
  where
    go crashes carts
      | M.size updated == 1 = (newCrashes, head . M.elems $ updated)
      | otherwise = go newCrashes updated
      where
        (newCrashes, updated) = runTick crashes paths carts

calc m = findCrashes paths cartsInit
  where
    paths = map (map removeCart) m
    cartsInit = foldl init M.empty grid
    grid = concat $ U.genGrid id (0, 0, length m - 1, (length . head $ m)-1)
    removeCart '^' = '|'
    removeCart 'v' = '|'
    removeCart '<' = '-'
    removeCart '>' = '-'
    removeCart c = c
    init carts (y,x)
      | m !! y !! x == '^' = M.insert (y,x) (Cart (y,x) Up TLeft) carts
      | m !! y !! x == 'v' = M.insert (y,x) (Cart (y,x) Down TLeft) carts
      | m !! y !! x == '<' = M.insert (y,x) (Cart (y,x) Left TLeft) carts
      | m !! y !! x == '>' = M.insert (y,x) (Cart (y,x) Right TLeft) carts
      | otherwise = carts

solve1 m = show x ++ "," ++ show y
  where
    (y,x) = last . fst $  calc m

solve2 m = show x ++ "," ++ show y
  where
    (y,x) = pos . snd . calc $ m

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
