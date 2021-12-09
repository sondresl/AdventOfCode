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
  loeb [ head . (lengths 6 \\) . ([(!! 6), (!! 9)] <*>) . pure
       , const . head $ lengths 2
       , head . (lengths 5 \\) . ([(!! 5), (!! 3)] <*>) . pure
       , \x -> unsafeFind (\fives -> all (`elem` fives) (x !! 1)) $ lengths 5
       , const . head $ lengths 4
       , \x -> unsafeFind (\threes -> all (`elem` threes) ((x !! 4) \\ (x !! 1))) $ lengths 5
       , \x -> unsafeFind (\sixes -> not $ all (`elem` sixes) (x !! 1)) $ lengths 6
       , const . head $ lengths 3
       , const . head $ lengths 7
       , \x -> unsafeFind (\sixes -> all (`elem` sixes) (x !! 4)) $ lengths 6
       ]
  where unsafeFind f = head . filter f
        lengths n = filter ((== n) . length) xs

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day08.in"
  let solve f = sum . map (\(signals, outputs) -> f $ map (determine signals Map.!) outputs)
  print $ solve (count (`elem` [1,4,7,8])) input
  print $ solve (unDigits 10) input
    
parseInput :: String -> [([String], [String])]
parseInput = map (f . words) . lines
  where
    f input = (map sort $ take 10 input, map sort $ drop 11 input)

-- 239
-- 946346
