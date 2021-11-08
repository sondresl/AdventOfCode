module Day20 where

import Lib
import Control.Lens
import Data.List.Extra
import qualified Data.Set as Set
import           Data.Set   ( Set )
import qualified Data.Map as Map
import           Data.Map   ( Map )
import Data.Foldable (foldl')
import Control.Monad.State

import Math.NumberTheory.ArithmeticFunctions

input :: Int
input = 34000000

part1 :: Maybe Int
part1 = (+1) <$> findIndex test [1..]
  where
    test n = (>= input) . foldl' (+) 0 . Set.map (*10) $ divisors n

part2 :: Maybe Int
part2 = fmap (+1) . findIndex (>= input) $ evalState (traverse test [1..]) Map.empty
  where
    test :: Int -> State (Map Int Int) Int
    test n = do
      seen <- get
      let divs = divisors n
          seen' = Map.unionWith (+) seen (freqs divs)
          divs' = Set.filter ((<= Just 50) . (`Map.lookup` seen')) divs
      put seen'
      pure $ foldl' (+) 0 . Set.map (*11) $ divs'

main :: IO ()
main = do
  print part1
  print part2

-- 786240
-- 831600
