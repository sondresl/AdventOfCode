module Day10 where

import Lib (display)
import Linear (V2(..))
import Data.Maybe (maybe, mapMaybe)
import Text.Read (readMaybe)
import Control.Monad (guard)

lightCRT :: (Int, Int) -> Maybe (V2 Int)
lightCRT (cycle, val) = guard inSprite *> Just (V2 col row)
  where
    (row, col) = (cycle - 1) `divMod` 40
    inSprite = col `elem` [pred val, val, succ val]

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day10.in"
  let res = scanl (flip ($)) 1 input
  print . sum . map ((*) <*> (res !!)) $ [20,60,100,140,180,220]
  putStrLn . display . mapMaybe lightCRT $ zip [0..] res

parseInput :: String -> [Int -> Int]
parseInput = (id:) . concatMap (map (maybe id (+) . readMaybe) . words) . lines

-- 14560
-- ▓▓▓▓ ▓  ▓ ▓▓▓  ▓  ▓ ▓▓▓▓ ▓▓▓  ▓  ▓ ▓▓▓▓
-- ▓    ▓ ▓  ▓  ▓ ▓  ▓ ▓    ▓  ▓ ▓  ▓    ▓
-- ▓▓▓  ▓▓   ▓  ▓ ▓▓▓▓ ▓▓▓  ▓  ▓ ▓  ▓   ▓
-- ▓    ▓ ▓  ▓▓▓  ▓  ▓ ▓    ▓▓▓  ▓  ▓  ▓
-- ▓    ▓ ▓  ▓ ▓  ▓  ▓ ▓    ▓    ▓  ▓ ▓
-- ▓▓▓▓ ▓  ▓ ▓  ▓ ▓  ▓ ▓▓▓▓ ▓     ▓▓  ▓▓▓▓
