module Day10 where

import Control.Lens
import Lib
import Data.List.Extra
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Data.Map (Map, (!))

data Bot = Bot Int | Output Int
  deriving (Show, Eq, Ord)
type Bots = Map Bot [Int]

data Instruction
  = Init Bot Int
  | Move Bot Bot Bot
  deriving Show

makeMap :: [Instruction] -> Bots
makeMap ins = result
  where
    result :: Bots
    result = foldl insert Map.empty ins
    insert :: Bots -> Instruction -> Bots
    insert acc (Init n value) = Map.insertWith (++) n [value] acc
    insert acc (Move bot low high) = 
      let curr = (Map.!) result bot
       in Map.insertWith (++) low [minimum curr] $ Map.insertWith (++) high [maximum curr] acc
 
part1 :: [Instruction] -> Maybe Bot
part1 = (fst <$>) . find ((== [17, 61]) . sort . snd) . Map.toList . makeMap

part2 :: [Instruction] -> Int
part2 = product . prod . makeMap
  where
    prod mp = concat [mp ! Output 0, mp ! Output 1, mp ! Output 2]

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        putStrLn ("\nInput file: " ++ show file ++ "\n")
        -- mapM_ print input
        print $ part1 input
        print $ part2 input

  run "../data/test"
  run "../data/day10.in"

-- Just (Bot 101)
-- 37789

parseInput :: String -> [Instruction]
parseInput = map (parseBots . words) . lines
  where
    parseBots ["value", n, _, _, _, bot] = Init (Bot (read bot)) (read n)
    parseBots ["bot",n,_,"low", _,"bot",low,_,_,_,"bot",high] = 
      Move (Bot (read n)) (Bot (read low)) (Bot (read high))
    parseBots ["bot",n,_,"low", _,"bot",low,_,_,_,"output",high] = 
      Move (Bot (read n)) (Bot (read low)) (Output (read high))
    parseBots ["bot",n,_,"low", _,"output",low,_,_,_,"bot",high] = 
      Move (Bot (read n)) (Output (read low)) (Bot (read high))
    parseBots ["bot",n,_,"low", _,"output",low,_,_,_,"output",high] = 
      Move (Bot (read n)) (Output (read low)) (Output (read high))
    parseBots n = error (show n)

