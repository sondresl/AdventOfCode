module Day21 where

import Lib (tuple, freqs, (.:))
import Control.Applicative (liftA3)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Sum(..))

type Frequencies = Map (Int, Int, Int, Int) Int

play :: (Int, Int) -> (Int, Int) -> Int -> [Int] -> Int
play (score1, pos1) (score2, pos2) cnt rolls =
  let turn = take 3 rolls
      pos' = (pos1 + sum turn) `mod` 10
      score' = score1 + pos' + 1
   in if score' >= 1000
         then score2 * (cnt + 3)
         else play (score2, pos2) (score', pos') (cnt + 3) (drop 3 rolls)


solve :: Frequencies -> Int -> Int
solve mp n = if Map.null unfin
                then getSum . uncurry max $ Map.foldMapWithKey findScores fin
                else solve (Map.unionWith (+) res fin) ((n + 1) `mod` 2)
  where
    rolls = Map.toList . freqs $ liftA3 (\a b c -> a + b + c) [1,2,3] [1,2,3] [1,2,3]
    done (_, a, _, b) _ = a >= 21 || b >= 21
    findScores (_, a, _, b) s = if a > b then (Sum s, 0) else (0, Sum s)
    res = Map.unionsWith (+) $ map (uncurry once) $ Map.toList unfin
    (fin, unfin) = Map.partitionWithKey done mp
    once start@(p1, s1, p2, s2) freq = Map.fromList $ do
      (v, freq') <- rolls
      let (pos, score, new) = case n of
                            0 -> ((p1 + v) `mod` 10, s1 + pos + 1, \a b -> (a, b, p2, s2))
                            1 -> ((p2 + v) `mod` 10, s2 + pos + 1, \a b -> (p1, s1, a, b))
      pure $ (new pos score, freq * freq')

main :: IO ()
main = do
  (one, two) <- parseInput <$> readFile "../data/day21.in"
  print $ play (0, one - 1) (0, two - 1) 0 (cycle [1..100])
  print $ solve (Map.singleton (one - 1, 0, two - 1, 0) 1) 0

parseInput :: String -> (Int, Int)
parseInput = tuple . map (read . pure . last) . lines

-- 752745
-- 309196008717909
