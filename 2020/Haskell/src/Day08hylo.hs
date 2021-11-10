module Day08hylo where

import Control.Comonad.Store (peeks)
import Control.Lens
import Data.Char (toUpper)
import Data.List (find)
import Data.Maybe (isNothing)
import Data.Vector (Vector, fromList)
import Lib (firstRepeatOn)
import Data.Functor.Foldable

-- Datatypes and parsing

data Instruction = Nop Int | Jmp Int | Acc Int
    deriving (Show, Eq, Ord, Read)
makePrisms ''Instruction

type Instructions = Vector Instruction

data Program = Program
    { _instructions :: Instructions
    , _ip :: Int
    , _acc :: Int
    }
    deriving (Show)
makeLenses ''Program

mkProgram :: Instructions -> Program
mkProgram is = Program is 0 0

parseInput :: String -> Instructions
parseInput = fromList . map (read . over _head toUpper) . lines . filter (/= '+')

data Log a
  = Running Int Int a
  | Terminated Int Int
  deriving (Show, Eq, Ord, Functor)

-- Main loop

run :: Program -> Log Program
run p@Program{..} = case _instructions ^? ix _ip of
    Just (Nop _) -> Running _ip _acc $ p & ip +~ 1
    Just (Jmp i) -> Running _ip _acc $ p & ip +~ i
    Just (Acc i) -> Running _ip _acc $ p & ip +~ 1 & acc +~ i
    Nothing -> Terminated _ip _acc

analyzeLog :: Log [(Int, Int)] -> [(Int, Int)]
analyzeLog (Running ip acc p) = (ip, acc) : p
analyzeLog (Terminated ip acc) = [(ip, acc)]

-- Day 8 specifics

changeInstructions :: Instructions -> [Instructions]
changeInstructions = map (peeks changeInst) . holesOf (traverse . filtered (hasn't _Acc))
  where
    changeInst (Jmp i) = Nop i
    changeInst (Nop i) = Jmp i
    changeInst n = n

part1 :: Instructions -> Maybe Int
part1 = fmap snd . firstRepeatOn fst . (analyzeLog `hylo` run) . mkProgram

part2 :: Instructions -> Maybe Int
part2 = fmap (snd . last) . removeRepeating . getOutput
  where
    removeRepeating = find (isNothing . firstRepeatOn fst)
    getOutput = map ((analyzeLog `hylo` run) . mkProgram) . changeInstructions

main :: IO ()
main = do
    input <- parseInput <$> readFile "../data/day08.in"
    print $ part1 input
    print $ part2 input

-- 1949
-- 2092
