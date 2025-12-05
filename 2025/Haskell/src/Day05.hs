module Day05 where

import Lib (count)
import Data.List.Extra (splitOn)
import qualified Data.Interval as I
import qualified Data.IntervalSet as IS

main :: IO ()
main = do
  (ranges, ids) <- parseInput <$> readFile "../data/day05.in"
  print $ count (`IS.member` ranges) ids
  print $ sum $ map ((+1) . I.width) $ IS.toList ranges

parseInput :: String -> (IS.IntervalSet Int, [Int])
parseInput input = (ranges, ids)
  where
    [top, bot] = splitOn "\n\n" input
    ranges = IS.fromList . map (toInter . map (read @Int) . splitOn "-") $ lines top
    toInter [x, y] = I.Finite x I.<=..<= I.Finite y 
    ids = map read (lines bot)

-- 679
-- 358155203664116
