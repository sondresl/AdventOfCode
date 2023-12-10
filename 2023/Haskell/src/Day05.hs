module Day05 where

import Lib (allNums)
import Data.Foldable (foldl')
import Data.List.Extra (splitOn, chunksOf)
import Data.IntervalMap.Strict (IntervalMap)
import qualified Data.IntervalMap.Strict as IM
import Data.Interval (Extended(Finite), (<=..<), lowerBound, mapMonotonic, singleton)

run :: [IntervalMap Int Int] -> IntervalMap Int Int -> Int
run maps seeds = res
  where 
    Finite res = minimum . map lowerBound $ IM.keys $ foldl' go seeds maps
    go ivs mp = found <> notFound
      where
        found = IM.fromList $ map (\(i, v) -> (mapMonotonic (+v) i, 0)) $ IM.toList $ IM.intersectionWith (+) ivs mp
        notFound = IM.map (const 0) $ IM.difference ivs mp

main :: IO ()
main = do
  (seeds, input) <- parseInput <$> readFile "../data/day05.in"
  print $ run input $ IM.fromList $ map ((,0) . singleton) seeds
  print $ run input $ IM.fromList $ map ((,0) . (\[s, r] -> s <=..< s + r)) $ chunksOf 2 $ map Finite seeds

parseInput :: String -> ([Int], [IntervalMap Int Int])
parseInput input = (allNums seeds, map IM.unions maps)
  where
  (seeds:rest) = splitOn "\n\n" input
  maps = map (mkRange . lines) rest
  mkRange (_:xs) = map (mkIntervalMap . map (read @Int) . words) xs
  mkIntervalMap [dest, src, range] = IM.singleton (IM.Finite src <=..< IM.Finite src + IM.Finite range) (dest - src)

-- 261668924
-- 24261545
