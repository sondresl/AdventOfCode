module Day05 where

import Lib
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

valid :: Map Int [Int] -> [Int] -> Bool
valid rules = all good . init . tails . reverse
  where
    good (x:xs) = disjoint rule xs
      where rule = Map.findWithDefault [] x rules

fix :: Map Int [Int] -> [Int] -> [Int]
fix (invertMap -> rules) = go 
  where
    go [] = []
    go (x:xs) =
      let rule = Set.fromList $ Map.findWithDefault [] x rules
          bad = filter (`Set.member` rule) xs
          good = filter (`Set.notMember` rule) xs
       in if null bad
            then x : go xs
            else go (bad <> [x] <> good)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day05.in"
  let (rules, pages) = input
  let (correct, incorrect) = partition (valid rules) pages
  print $ sum $ map middle correct
  print $ sum $ map (middle . fix rules) incorrect

parseInput :: String -> (Map Int [Int], [[Int]])
parseInput input = (rules, pages)
  where
    [top, bot] = splitOn "\n\n" input
    rules = Map.unionsWith (<>) $ do
      (from, to) <- map (tuple . allNums) $ lines top
      pure $ Map.singleton from [to]
    pages = map allNums $ lines bot

-- 4959
-- 4655
