module Day12 where

import Lib (tuple, allNums)
import Data.List.Extra (intercalate)
import Data.MemoTrie (memo2)
import Data.Tuple.Extra (second)

parseRecord :: String -> [Int] -> Int
parseRecord = memo2 $ \str ints -> go str ints
  where
    go "" xs = if null xs then 1 else 0
    go str [] = if all (`elem` "?.") str then 1 else 0
    go str ints | sum ints + (length ints - 1) > length str = 0
    go ('.':str) xs = parseRecord str xs
    go ('#':str) (x:xs) | '.' `elem` take (pred x) str = 0
    go ('#':str) (x:xs)
      | length (take x ('#':str)) == x 
        && all (`elem` "?#") (take (x - 1) str)
        && (null (drop (x - 1) str) || str !! pred x `elem` "?.")
         = parseRecord (drop x str) xs
    go ('#':str) (x:xs) = 0
    go ('?':str) (x:xs) = parseRecord ('#':str) (x:xs) + parseRecord str (x:xs)
    go a b = error $ show a <> " | " <> show b

part2prep :: (String, [Int]) -> (String, [Int])
part2prep (str, ints) = (str', ints')
  where
    ints' = concat $ replicate 5 ints
    str' = intercalate "?" $ replicate 5 str

main :: IO ()
main = do
  input <- map (second allNums . tuple . words) . lines <$> readFile "../data/day12.in"
  print . sum $ map (uncurry parseRecord) input
  print . sum $ map (uncurry parseRecord . part2prep) input

-- 7718
-- 128741994134728
