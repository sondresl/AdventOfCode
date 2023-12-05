module Day05 where

import Lib (allNums)
import Data.List.Extra ( splitOn, find, chunksOf )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

findLocation :: [[[Integer]]] -> Integer -> Integer
findLocation allMaps initialSeed = foldl go initialSeed allMaps
  where
    go seed maps = case find inRange maps of
                     Nothing -> seed
                     Just [dest, src, range] -> dest + (seed - src)
      where
        inRange [dest, src, range] = src <= seed && seed <= src + range - 1

isSeed :: [Integer] -> Integer -> Bool
isSeed (chunksOf 2 -> pairs) cand = 
  any (\[start, range] -> start <= cand && cand <= start + range - 1) pairs

findSeed :: [[[Integer]]] -> Integer -> Integer
findSeed allMaps initialLoc = foldl go initialLoc allMaps
  where
    go loc maps = case find inRange maps of
                     Nothing -> loc
                     Just [dest, src, range] -> src + (loc - dest)
      where
        inRange [dest, src, range] = dest <= loc && loc <= dest + range - 1
        
main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day05.in"
  let (seeds, maps) = input
  print $ minimum $ map (findLocation maps) seeds
  let revMaps = reverse maps
  -- Original brute force search, set the start number gradually higher to control brute force
  -- let start = 20000000
  -- print $ find (isSeed seeds . snd) $ zip [start..] $ map (findSeed revMaps) [start..start + 10000000]
  print $ isSeed seeds $ findSeed revMaps 24261546

parseInput :: String -> ([Integer], [[[Integer]]])
parseInput input = (seeds, maps)
  where
  seeds :: [Integer]
  seeds = allNums ss
  (ss:rest) = splitOn "\n\n" input
  maps = map (mkRange . lines) rest
  mkRange (_:xs) = map (map read . words) xs

-- 261668924
-- 24261546

