module Day05 where

import Lib (allNums, middle, tuple)
import Data.List.Extra (partition, sortBy, splitOn, sumOn')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bool (bool)

-- The input probably provides the complete set of rules, so it is possible to
-- sort by checking if a letter appears in the set of letters that the other
-- letter has to be in front of.
orderNums :: Map Int (Set Int) -> Int -> Int -> Ordering
orderNums rules a b = bool LT GT (b `Set.member` Map.findWithDefault Set.empty a rules)

main :: IO ()
main = do
  (rules, pages) <- parseInput <$> readFile "../data/day05.in"
  let comp = orderNums rules
  let (sorted, unsorted) = partition ((==) <*> sortBy comp) pages
  print $ sumOn' middle sorted
  print $ sumOn' (middle . sortBy comp) unsorted

parseInput :: String -> (Map Int (Set Int), [[Int]])
parseInput input = (rules, pages)
  where
    [top, bot] = splitOn "\n\n" input
    rules = Map.unionsWith (<>) $ do
      [from, to] <- map allNums $ lines top
      pure $ Map.singleton to (Set.singleton from)
    pages = map allNums $ lines bot

-- 4959
-- 4655
