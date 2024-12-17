module Day07 where

import Control.Lens (Bifunctor (bimap), view, _1, _2, _3)
import Data.Function (on)
import Data.List.Extra (group, maximumBy, sort, sortBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down (Down))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (first, second)
import Lib (count, perturbations, tuple)

data Type = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A
  deriving (Show, Eq, Ord, Read, Enum)

data Rank = High | Pair | TwoPairs | ThreeEq | House | FourEq | FiveEq
  deriving (Show, Eq, Ord)

type Hand = [Type]

bestSwap :: Hand -> Rank
bestSwap xs | all (== Joker) xs = FiveEq
bestSwap xs = rank $ replicate js mostCommon <> filter (/= Joker) xs
  where
    js = count (== Joker) xs
    mostCommon = head . head . sortOn (negate . length) . group . sort $ filter (/= Joker) xs

rank :: Hand -> Rank
rank card =
  case sort . map length $ group (sortOn Down card) of
    [5] -> FiveEq
    [1, 4] -> FourEq
    [2, 3] -> House
    [1, 1, 3] -> ThreeEq
    [1, 2, 2] -> TwoPairs
    [1, 1, 1, 2] -> Pair
    [1, 1, 1, 1, 1] -> High

run :: [(Hand, Rank, Int)] -> Int
run = sum . zipWith (*) [1 ..] . map (view _3) . sortBy bestHand
  where bestHand = (compare `on` view _2) <> (compare `on` view _1)

main :: IO ()
main = do
  (cards, vals) <- unzip . parseInput <$> readFile "../data/day07.in"

  print $ run $ zip3 cards (map rank cards) vals

  let toJoker J = Joker
      toJoker n = n

      jokers = map (map toJoker) cards
      swappedRanks = map bestSwap jokers

  print $ run $ zip3 jokers swappedRanks vals


parseInput :: String -> [(Hand, Int)]
parseInput = map (bimap (map toType) read . tuple . words) . lines
  where
    toType = (Map.fromList (zip "23456789TJQKA" [Two .. A]) Map.!)

-- 247823654
-- 245461700
