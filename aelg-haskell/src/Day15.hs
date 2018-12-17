module Day15 ( solve ) where

import           Control.Arrow
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe
import qualified Utils as U

data Type = Elf | Goblin deriving (Show, Eq)

data Tile = Wall | Open deriving (Show, Eq)

data Creature = Creature { _race :: Type
                         , _hp :: Int
                         , _attack :: Int
                         , _pos :: (Int, Int)} deriving Show

parse input = foldl readPos (M.empty, M.empty) coords
  where
    coords = concat $ U.genGrid id (0, 0, length (head input) - 1, length input - 1)
    readPos (room, creatures) (y,x) = (M.insert (y,x) (tile (y,x)) room, M.alter (creature (y,x)) (y,x) creatures)
    tile (y,x)
      | input !! y !! x == '#' = Wall
      | otherwise = Open
    creature (y,x)
      | input !! y !! x == 'E' = const $ Just $ Creature Elf 200 3 (y,x)
      | input !! y !! x == 'G' = const $ Just $ Creature Goblin 200 3 (y,x)
      | otherwise = const Nothing

pprint (room, creatures) = unlines $ map (map p) coords
  where
    maxY = maximum . map fst $ M.keys room
    maxX = maximum . map snd $ M.keys room
    coords = U.genGrid id (0, 0, maxX, maxY)
    p (x,y)
      | isJust creature = if _race (fromJust creature) == Elf then 'E' else 'G'
      | otherwise = if room M.! (y,x) == Open then '.' else '#'
      where creature = M.lookup (y,x) creatures

isElf (Creature Elf _ _ _) = True
isElf _ = False

runAll rounds (room, creatures)
  | done = (rounds, r, c)
  | otherwise = runAll (rounds + 1) (r,c)
  where (done, (r, c)) = runRound (room, creatures)

runRound (room, creaturesa) = go (room, creaturesa) $ M.keys creaturesa
  where
    go (room, creatures) (x:xs)
      | M.null . M.filter isElf $ creatures = (True, (room, creatures))
      | M.null . M.filter (not . isElf) $ creatures = (True, (room, creatures))
      | otherwise = go (runMove (room, creatures) (creatures M.!? x)) xs
    go a [] = (False , a)

adjacent (y,x) = [(y-1,x), (y,x-1), (y,x+1), (y+1,x)]

runMove a Nothing = a
runMove (room, creatures) (Just creature@(Creature race hp _ pos)) = attack newCreature doMove
  where
    (newCreature, doMove) = bestMove targetTile creature (room, creatures)
    targetTiles = S.fromList . concatMap (adjacent . _pos) . filter ((/= race) . _race) $ M.elems creatures
    targetTile = listToMaybe . sortOn snd . M.toList $ M.restrictKeys shortestPaths targetTiles
    shortestPaths = U.bfs moves [pos]
    moves pos = filter go (adjacent pos)
      where
        go pos = isNothing (M.lookup pos creatures) && (Open == room M.! pos)

attack creature (room,creatures) = (room, update weakest)
  where
    attacked opponent
      | _hp opponent > _attack creature = Just $ opponent{ _hp = _hp opponent - _attack creature}
      | otherwise = Nothing
    update Nothing = creatures
    update (Just opponent) = M.update attacked (_pos opponent) creatures
    weakest = listToMaybe . sortOn _hp $ map (creatures M.!) adjacentOpponents 
    adjacentOpponents = filter (go . (creatures M.!?)) (adjacent (_pos creature))
      where
        go (Just opponent) = _race opponent /= _race creature
        go Nothing = False

bestMove Nothing c a = (c,a)
bestMove (Just (_,0)) c a = (c,a)
bestMove (Just (targetTile, w)) creature (room, creatures) = (newCreature ,(room, newCreatures))
  where
    taken pos = isNothing (M.lookup pos creatures) && (Open == room M.! pos)
    (newCreature, newCreatures) = foldr go (creature, creatures) $ filter taken $ adjacent (_pos creature)
      where
        go pos acc
          | isNothing shortestPath = acc
          | fromJust shortestPath == w-1 = (newCreature, update pos)
          | otherwise = acc
          where 
            shortestPath = M.lookup targetTile shortestPaths
            shortestPaths = U.bfs moves [pos]
            moves pos = filter taken (adjacent pos)
            newCreature = creature{ _pos = pos}
            update pos = M.insert pos newCreature $  M.delete (_pos creature) creatures

hitpoints = sum . map _hp . M.elems

solve1 (room, creatures) = show $ totHp*rounds
  where
    totHp = hitpoints finalCreatures
    (rounds, _, finalCreatures) = runAll 0 (room, creatures)

testHitpower minAttack maxAttack (room, creatures)
  | minAttack == maxAttack && elfDied = testHitpower maxAttack (maxAttack*2) (room, creatures)
  | minAttack == maxAttack = (rounds, finalCreatures)
  | elfDied = testHitpower (attack+1) maxAttack (room, creatures)
  | otherwise = testHitpower minAttack attack (room, creatures)
  where
    attack = (maxAttack + minAttack) `div` 2
    elfDied = M.size (M.filter isElf finalCreatures) /=  M.size (M.filter isElf creatures)
    modifiedElves = fmap setAttack creatures
    setAttack c@(Creature Elf _ _ _) = c{_attack = attack}
    setAttack c = c
    (rounds, _, finalCreatures) = runAll 0 (room, modifiedElves)

solve2 (room, creatures) = show $ totHp*rounds
  where
    totHp = hitpoints finalCreatures
    (rounds, finalCreatures) = testHitpower 3 10000 (room, creatures)

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
