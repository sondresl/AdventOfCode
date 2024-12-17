module Day08 where

import Lib (findLoopSimple, takeUntil, tuple)
import Data.List.Extra (splitOn)
import Data.Tuple.Extra (second)
import Data.Map (Map)
import qualified Data.Map as Map

type LookupTable = Map String (String, String)

steps :: LookupTable -> String -> [(Int, (String, String) -> String)] -> [(Int, String)]
steps input start (m:moves) = (fst m, next) : steps input next moves
  where Just next = snd m <$> Map.lookup start input

part1 :: LookupTable -> [(Int, (String, String) -> String)] -> Int
part1 maps = length . takeUntil ((== "ZZZ") . snd) . steps maps "AAA"

part2 :: LookupTable -> [(Int, (String, String) -> String)] -> Int
part2 input dirs = foldl1 lcm $ map (snd . findLoopSimple . seq) starts
  where 
    starts = filter ((== 'A') . last) $ Map.keys input
    seq s = steps input s dirs

main :: IO ()
main = do
  (cycle . zip [1..] -> dirs, maps) <- parseInput <$> readFile "../data/day08.in"
  print $ part1 maps dirs
  print $ part2 maps dirs

parseInput :: String -> ([(String,String) -> String], LookupTable)
parseInput input = (dirs', maps')
  where 
    (dirs, maps) = tuple $ splitOn "\n\n" input
    dirs' = map (\x -> let Just v = lookup x [('R', snd), ('L', fst)] in v) dirs
    maps' = Map.fromList . map (second f . tuple . splitOn " = ") $ lines maps
    f (tail . init -> str) = tuple $ splitOn ", " str

-- 11567
-- 9858474970153
