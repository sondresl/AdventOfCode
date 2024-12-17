{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Extra
import Control.Lens
import Text.ParserCombinators.Parsec
import Data.Functor.Foldable

import Data.Tree

import Debug.Trace

type Graph = Tree (String, Int)
type Disc = (String, Int, [String])

parseInput :: String -> [Disc]
parseInput = either (error . show) id . traverse (parse parseDiscs "") . lines
  where
    parseDiscs = do
      name <- many1 letter <* space <* char '('
      weight <- many1 digit <* char ')'
      rest <- try (string " -> " *> sepBy1 (many1 letter) (string ", ")) <|> pure []
      pure (name, read @Int weight, rest)

mkGraph :: [Disc] -> Map String (Int, [String])
mkGraph = Map.fromList . map (\(name, weight, paths) -> (name, (weight, paths)))

findRoot :: [Disc] -> String
findRoot (mkGraph -> graph) = minimum . foldr (flip $ foldr Set.delete) names . map (snd . snd) $ Map.toList graph 
  where names = Map.keysSet graph
      
mkTree :: [Disc] -> Graph
mkTree lst = go root
  where
    root = findRoot lst
    graph = mkGraph lst
    go str = Node (str, wht) (map go children)
      where
        (wht, children) = graph Map.! str

findDiff :: [(Int, Int)] -> Int
findDiff xs = let Just x = lookup res (map swap xs) in if res > k then x - diff else x + diff
  where
    res = fst . head . filter ((== 1) . snd) . Map.toList $ freqs
    (k, v) = Map.findMin $ Map.delete res freqs
    diff = abs (res - k)
    freqs = Map.fromListWith (+) . flip zip (repeat 1) $ map snd xs
    swap (x,y) = (y,x)

part1 :: [Disc] -> String
part1 = findRoot

part2 :: [Disc] -> Int
part2 (mkTree -> tree) = either id (error . show) $ go tree
  where
    go (Node (name, wht) []) = Right (wht, wht)
    go (Node (name, wht) children) = 
      case traverse go children of 
        Left x -> Left x
        Right xs -> if not . all (uncurry (==)) . (zip <*> tail) . map snd $ xs
                        then Left $ findDiff xs
                        else Right . (wht,) . (+ wht) . sum . map snd $ xs

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/07.in"
  putStrLn $ part1 input
  print $ part2 input
