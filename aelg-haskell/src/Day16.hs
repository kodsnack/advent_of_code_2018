module Day16 ( solve ) where

import           Control.Arrow
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Bits
import           Text.ParserCombinators.ReadP
import qualified Parsing as P

data Instruction = Instruction Int Int Int Int deriving Show

type Registers = [Int]

data Test = Test Registers Registers Instruction deriving Show

parser = do
  tests <- many parseTest
  char '\n'
  program <- many parseInstruction
  eof
  return (tests, program)
  where 
    parseInstruction = do
      skipSpaces
      code <- P.integerAnd (char ' ')
      a <- P.integerAnd (char ' ')
      b <- P.integerAnd (char ' ')
      c <- P.integerAnd (char '\n')
      return $ Instruction code a b c
    parseTest = do
      string "Before: "
      before <- readS_to_P reads
      skipSpaces
      instruction <- parseInstruction
      string "After: "
      after <- readS_to_P reads
      string "\n\n"
      return $ Test before after instruction 

parse = P.run parser . unlines

updateReg 0 v [r0, r1, r2, r3] = [v, r1, r2, r3]
updateReg 1 v [r0, r1, r2, r3] = [r0, v, r2, r3]
updateReg 2 v [r0, r1, r2, r3] = [r0, r1, v, r3]
updateReg 3 v [r0, r1, r2, r3] = [r0, r1, r2, v]

getReg r i = i !! r

rr op (Instruction _ a b _) reg = op (getReg a reg) (getReg b reg)
ri op (Instruction _ a b _) reg = op (getReg a reg) b
ir op (Instruction _ a b _) reg = op a (getReg b reg)

toInt op a b
  | op a b = 1
  | otherwise = 0

runOp op instr@(Instruction _ _ _ c) reg = updateReg c (op instr reg) reg

operations =
  [ rr (+)
  , ri (+)
  , rr (*)
  , ri (*)
  , rr (.&.)
  , ri (.&.)
  , rr (.|.)
  , ri (.|.)
  , rr const
  , ir const
  , rr (toInt (>))
  , ri (toInt (>))
  , ir (toInt (>))
  , rr (toInt (==))
  , ri (toInt (==))
  , ir (toInt (==))
  ]

solve1 = show . length . filter (>2) . map matching . fst
  where
    matching (Test before after instr) = length . filter (== after) $ map (\op -> runOp op instr before) operations

possibleOpcodes :: [Test] -> M.Map Int [Int]
possibleOpcodes = foldl update initial . map opCodeMatch
  where
    opCodeMatch test@(Test before after (Instruction instr _ _ _)) = (instr, matching test)
    matching (Test before after instr) = map fst . filter ((== after) . snd) . zip [0..] $ map (\op -> runOp op instr before) operations
    update m (opcode, match) = M.update (Just . intersect match) opcode m
    initial = M.fromList . zip [0..] $ replicate (length operations) [0 .. length operations - 1]

opCodes :: [Test] -> M.Map Int Int
opCodes = fmap head . resolvOpcodes . possibleOpcodes
  where
    resolvOpcodes m = head . dropWhile ((>1) . maximum . fmap length)  $ iterate removeSingleMatch m
    removeSingleMatch m = fmap go m
      where
        go [x] = [x]
        go xs = filter (not . (`elem` singles m)) xs
    singles m = concat . M.elems $ M.filter ((== 1) . length) m

solve2 input = show . head $ runProgram program
  where
    (tests, program) = input
    opCodeMap = opCodes tests
    runProgram = foldl runStep [0,0,0,0]
    runStep reg instr@(Instruction opCode _ _ _) = runOp (operations !! fromJust (M.lookup opCode opCodeMap)) instr reg


solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
