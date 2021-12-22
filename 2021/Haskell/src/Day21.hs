module Day21 where

import Lib (tuple, freqs, memo4)
import Linear (_x, V2(..))
import Advent.Coord (invert)
import Control.Lens (view)
import Control.Applicative (liftA3)
import Data.Map (Map)
import qualified Data.Map as Map

play :: (Int, Int) -> (Int, Int) -> Int -> [Int] -> Int
play (score1, pos1) (score2, pos2) cnt rolls
  | score' >= 1000 = score2 * (cnt + 3)
  | otherwise = play (score2, pos2) (score', pos') (cnt + 3) (drop 3 rolls)
  where
    pos' = (pos1 + sum (take 3 rolls)) `mod` 10
    score' = score1 + pos' + 1

solve :: Int -> Int -> Int -> Int -> V2 Int
solve = memo4 $ \p1 s1 p2 s2 -> sum $ do
  (v, freq) <- Map.toList (freqs (liftA3 (\a b c -> a + b + c) [1,2,3] [1,2,3] [1,2,3]))
  let p1' = (p1 + v) `mod` 10
      s1' = s1 + p1' + 1
  pure $ if s1' >= 21 
           then V2 freq 0
           else (* V2 freq freq) . invert $ solve p2 s2 p1' s1'

main :: IO ()
main = do
  (one, two) <- parseInput <$> readFile "../data/day21.in"
  print $ play (0, one - 1) (0, two - 1) 0 (cycle [1..100])
  print $ view _x $ solve (one - 1) 0 (two - 1) 0

parseInput :: String -> (Int, Int)
parseInput = tuple . map (read . pure . last) . lines

-- 752745
-- 309196008717909
