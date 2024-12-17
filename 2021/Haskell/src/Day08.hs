module Day08 where

import Lib (count)
import Data.List.Extra (sort, delete, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Digits (unDigits)

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

determine :: [String] -> Map String Int
determine xs = Map.fromList $ flip zip [0..] $
  loeb [ \x -> head $ delete (x !! 6) $ delete (x !! 9) $ filter ((== 6) . length) xs -- zero
       , const $ head $ filter ((== 2) . length) xs -- one
       , \x -> head $ delete (x !! 5) $ delete (x !! 3) $ filter ((== 5) . length) xs -- two
       , \x -> head $ filter (\fives -> all (`elem` fives) (x !! 1)) $ filter ((== 5) . length) xs -- three
       , const $ head $ filter ((== 4) . length) xs -- four
       , \x -> head $ filter (\threes -> all (`elem` threes) ((x !! 4) \\ (x !! 1))) $ filter ((== 5) . length) xs -- five
       , \x -> head $ filter (\sixes -> not $ all (`elem` sixes) (x !! 1)) $ filter ((== 6) . length) xs -- six
       , const $ head $ filter ((== 3) . length) xs -- seven
       , const $ head $ filter ((== 7) . length) xs -- eight
       , \x -> head $ filter (\sixes -> all (`elem` sixes) (x !! 4)) $ filter ((== 6) . length) xs -- nine
       ]

solve :: ([Int] -> Int) -> [([String], [String])] -> Int
solve f = sum . map run
  where
    run (signals, outputs) = f $ map (res Map.!) outputs
      where
        res = determine signals

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day08.in"
  print $ solve (count (`elem` [1,4,7,8])) input
  print $ solve (unDigits 10) input
    
parseInput :: String -> [([String], [String])]
parseInput = map (f . words) . lines
  where
    f input = (map sort $ take 10 input, map sort $ drop 11 input)
