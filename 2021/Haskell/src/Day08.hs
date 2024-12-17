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
  loeb [ head . (filter ((== 6) . length) xs \\) . ([(!! 6), (!! 9)] <*>) . pure -- zero
       , const $ unsafeFind ((== 2) . length) xs -- one
       , head . (filter ((== 5) . length) xs \\) . ([(!! 5), (!! 3)] <*>) . pure -- two
       , \x -> unsafeFind (\fives -> all (`elem` fives) (x !! 1)) $ filter ((== 5) . length) xs -- three
       , const $ unsafeFind ((== 4) . length) xs -- four
       , \x -> unsafeFind (\threes -> all (`elem` threes) ((x !! 4) \\ (x !! 1))) $ filter ((== 5) . length) xs -- five
       , \x -> unsafeFind (\sixes -> not $ all (`elem` sixes) (x !! 1)) $ filter ((== 6) . length) xs -- six
       , const $ unsafeFind ((== 3) . length) xs -- seven
       , const $ unsafeFind ((== 7) . length) xs -- eight
       , \x -> unsafeFind (\sixes -> all (`elem` sixes) (x !! 4)) $ filter ((== 6) . length) xs -- nine
       ]
  where unsafeFind f = head . filter f

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
