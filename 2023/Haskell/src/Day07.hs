module Day07 where

import Lib
import Advent.Coord
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count)
import Data.Tuple.Extra (second, first)
import Data.Ord (Down(Down))
import Data.Function (on)

data Type = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A
  deriving (Show, Eq, Ord, Read, Enum)

data Rank = High | Pair | TwoPairs | ThreeEq | House | FourEq | FiveEq
  deriving (Show, Eq, Ord)

type Hand = [Type]

toType :: Char -> Type
toType '2' = Two
toType '3' = Three
toType '4' = Four
toType '5' = Five
toType '6' = Six
toType '7' = Seven
toType '8' = Eight
toType '9' = Nine
toType c = read $ pure c

bestSwap :: Hand -> Hand
bestSwap xs | count (==Joker) xs == 5 = [A,A,A,A,A]
bestSwap xs | count (==Joker) xs == 4 = let [x] = filter (/= Joker) xs in [x,x,x,x,x]
bestSwap xs | count (==Joker) xs == 3 = let [low, high] = sort $ filter (/= Joker) xs in [low,high,high,high,high]
bestSwap hand = maximumBy bestHand $ new hand
  where
    new = concat . take 3 . iterate f . pure
    f = concatMap (perturbations swap)
    swap Joker = [Two, Three, Four, Five, Six, Seven, Eight, Nine, T, Q, K, A]
    swap _ = []

rank :: Hand -> Rank
rank card = let c = group (sortOn Down card)
             in case c of
                  [x] -> FiveEq
                  xs | any ((==4) . length) xs -> FourEq
                  [x, y] | length x == 3 && length y == 2 -> House
                  [x, y] | length y == 3 && length x == 2 -> House
                  xs | any ((==3) . length) xs -> ThreeEq
                  [x, y, z] | length x == 2 && length y == 2 -> TwoPairs
                  [x, y, z] | length x == 2 && length z == 2 -> TwoPairs
                  [x, y, z] | length y == 2 && length z == 2 -> TwoPairs
                  xs | any ((==2) . length) xs -> Pair
                  xs -> High

bestHand :: Hand -> Hand -> Ordering
bestHand = (compare `on` rank) <> compare

bestHand' :: (Hand, Hand) -> (Hand, Hand) -> Ordering
bestHand' = (compare `on` rank . snd) <> (compare `on` fst)

part1 :: [(Hand, Int)] -> Int
part1 = sum . zipWith (*) [1..] . map snd . sortBy (bestHand `on` fst)

part2 :: [(Hand, Int)] -> Int
part2 = sum . zipWith (*) [1..] . map snd . sortBy (bestHand' `on` fst) . map g . map (first (map toJoker))
  where
    toJoker J = Joker 
    toJoker n = n
    g (card, val) = ((card, bestSwap card), val)

main :: IO ()
main = do

  let run str input = do
        putStrLn str
        print $ part1 input
        print $ part2 input
    
  run "\nTest:\n\n" $ parseInput testInput

  input <- parseInput <$> readFile "../data/day07.in"
  run "\nActual:\n\n" input

-- 247823654
-- 245461700

parseInput :: String -> [(Hand, Int)]
parseInput = map (bimap (map toType) read . tuple . words) . lines

testInput = [r|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
|]
