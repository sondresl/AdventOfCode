module Day21 where

import Lib (tuple, freqs)
import Control.Applicative (liftA3)
import Data.List.Extra (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

play :: ((Int, Int), (Int, Int)) -> Int -> [Int] -> Int
play ((score1, pos1), (score2, pos2)) cnt rolls =
  let turn = take 3 rolls
      pos' = (pos1 + sum turn) `mod` 10
      score' = score1 + pos' + 1
   in if score' >= 1000
         then score2 * (cnt + 3)
         else play ((score2, pos2), (score', pos')) (cnt + 3) (drop 3 rolls)

outcomes :: Int -> Int -> Map Int (Int, Int) -> [(Int, Int)]
outcomes turn worlds mp =
  let fr = sortBy (flip compare) . Map.toList . freqs $ liftA3 (\a b c -> a + b + c) [1,2,3] [1,2,3] [1,2,3]
   in do
     (val, freq) <- fr
     let (score, pos) = mp Map.! turn
         pos' = (pos + val) `mod` 10
         score' =  score + pos' + 1
     if score' >= 21 
        then pure (turn, worlds * freq)
        else outcomes ((turn + 1) `mod` 2) (worlds * freq) (Map.insert turn (score', pos') mp)

main :: IO ()
main = do
  (one, two) <- parseInput <$> readFile "../data/day21.in"
  -- let (one, two) = (6,3)
  print $ (one, two)
  print $ play ((0, one - 1), (0, two - 1)) 0 (cycle [1..100])
  print . Map.fromListWith (+) 
        . outcomes 0 1 
        $ Map.fromList [(0, (0, one - 1)), (1, (0, two - 1))]

parseInput :: String -> (Int, Int)
parseInput = tuple . map (read . pure . last) . lines

-- 752745
-- 309196008717909
