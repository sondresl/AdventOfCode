{-# LANGUAGE TupleSections #-}
module Day14 where

import Lib (freqs, minimumVal', maximumVal', zipWithTail)
import Data.Maybe (fromJust)
import Control.Applicative (liftA2)
import Data.List.Extra (splitOn)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map

type Rules = Map (Char, Char) Char
type Count = Map (Char, Char) Integer 

-- | Replace n instances of p with n new pairs
go :: Rules -> Count -> (Char, Char) -> [((Char, Char), Integer)]
go rules cnt p@(x,y) = 
  let currCount = Map.findWithDefault 0 p cnt 
   in case Map.lookup p rules of
        Nothing -> [(p, currCount)]
        Just v -> [(p, 0), ((x,v), currCount), ((v, y), currCount)]

react :: Rules -> Count -> Count
react rules cnt = Map.fromListWith (+) . concatMap (go rules cnt) $ Map.keys cnt

main :: IO ()
main = do
  (start, rules) <- parseInput <$> readFile "../data/day14.in"
  let initial = freqs . zipWithTail
      countElems = Map.adjust (+1) (last start)
                 . Map.fromListWith (+) 
                 . map (\((x,_), v) -> (x, v)) 
                 . Map.toList
      ans v = liftA2 ((-) `on` snd) maximumVal' minimumVal' (countElems v)
      gens = iterate (react rules) (initial start)
  print . ans . (!! 10) $ gens
  print . ans . (!! 40) $ gens

parseInput :: String -> (String, Rules)
parseInput = f . splitOn "\n\n"
  where
    f (x:xs) = (x, Map.fromList . map g $ lines $ head xs)
    g = (\[(a:b:_),y] -> ((a, b), head y)) . splitOn " -> "

-- 2621
-- 2843834241366
