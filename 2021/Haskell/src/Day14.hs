{-# LANGUAGE TupleSections #-}
module Day14 where

import Lib (iterateN, freqs, zipWithTail)
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

type Rules = Map (Char, Char) Char
type Count = Map (Char, Char) Integer 

-- | Replace n instances of p with n new pairs
grow :: Rules -> Count -> (Char, Char) -> Count
grow rules count p@(x,y) = 
  let currCount = Map.findWithDefault 0 p count 
      new = rules Map.! p
   in Map.fromList [(p, 0), ((x, new), currCount), ((new, y), currCount)]

main :: IO ()
main = do
  (start, rules) <- parseInput <$> readFile "../data/day14.in"
  let countElems = Map.adjust (+ 1) (last start)
                 . Map.foldrWithKey (\(k, _) v acc -> Map.insertWith (+) k v acc) 
                                    Map.empty
      react rules = Map.unionsWith (+) . (map . grow rules <*> Map.keys)
      gens n = ((-) . maximum <*> minimum)
             . countElems
             . iterateN n (react rules) 
             . freqs 
             $ zipWithTail start
  print $ gens 10
  print $ gens 40

parseInput :: String -> (String, Rules)
parseInput = f . splitOn "\n\n"
  where
    f (x:xs:_) = (x, foldMap g $ lines xs)
    g = (\[a:b:_,y] -> Map.singleton (a, b) (head y)) . splitOn " -> "

-- 2621
-- 2843834241366
