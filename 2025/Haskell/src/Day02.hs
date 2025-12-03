module Day02 where

import Lib (tuple)
import Data.List.Extra (splitOn, chunksOf, nub)
import qualified Data.Interval as I

repeats :: Int -> [Integer]
repeats n = concatMap go [2 .. n]
  where
    go reps = takeWhile (< 10_000_000_000) $ map (repeat reps) [1..]
    repeat :: Int -> Int -> Integer
    repeat reps = read . concat . replicate reps . show

main :: IO ()
main = do
  intervals <- parseInput' <$> readFile "../data/day02.in"
  let run = sum . nub . filter (\n -> any (I.member n) intervals)
  print $ run (repeats 2)
  print $ run (repeats 11)

parseInput' :: String -> [I.Interval Integer]
parseInput' = map f . map (tuple . map read . splitOn "-") . splitOn ","
  where f (from, to) = (I.Finite from) I.<=..<= (I.Finite to)

-- 29818212493
-- 37432260594
