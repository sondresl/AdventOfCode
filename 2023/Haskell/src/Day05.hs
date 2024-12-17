module Day05 where

import Lib (allNums, binaryMinSearch)
import Data.Foldable (foldl')
import Data.List.Extra ( splitOn, find, chunksOf )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntervalMap.Strict as IM
import Data.Interval
import Text.RawString.QQ

findLocation :: [[[Integer]]] -> Integer -> Integer
findLocation allMaps initialSeed = foldl' go initialSeed allMaps
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

-- mkInterval :: [Extended Integer] -> (IntegerInterval, IntegerInterval)
-- mkInterval [dest, src, range] = (src <=..< src + range, dest <=..< dest + range)

main :: IO ()
main = do
  print $ IM.singleton (98 <=..< 98 + 2) (50 - 98)
  -- (seeds, input) <- parseInput <$> readFile "../data/day05.in"
  -- let (seeds, input) = parseInput testInput
  -- print seeds
  -- print $ head input
  -- print $ combineIntervals (14 <=..< 20) (14 <=..< 20, 50 <=..< 56)
  -- print $ intersection (14 <=..< 20) (18 <=..< 56)
  -- let (seeds, maps) = input
  -- print $ minimum $ map (findLocation maps) seeds
  -- let revMaps = reverse maps
  -- print $ member 20 $ 10 <=..< 20
  -- Original brute force search, set the start number gradually higher to control brute force
  -- let start = 20000000
  -- print $ find (isSeed seeds . snd) $ zip [start..] $ map (findSeed revMaps) [start..start + 10000000]
  -- print $ isSeed seeds $ findSeed revMaps 24261546
  -- print $ binaryMinSearch (isSeed seeds . findSeed revMaps . fromIntegral) 1 100000000

-- -- parseInput :: String -> ([Integer], [[[Integer]]])
-- parseInput :: String -> ([IntegerInterval], [[(IntegerInterval, IntegerInterval)]])
-- parseInput input = (map singleton seeds, map (map mkInterval) maps)
--   where
--   seeds :: [Integer]
--   seeds = allNums ss
--   (ss:rest) = splitOn "\n\n" input
--   maps = map (mkRange . lines) rest
--   mkRange (_:xs) = map (map (Finite . read @Integer) . words) xs

-- 261668924
-- 24261545

testInput = [r|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|]
