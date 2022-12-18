module Day16 where

import Lib (allNums, dfs)
import Control.Monad (guard)
import Data.List.Extra (maximumOn, nubOrdOn, tails, groupOn, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Volcano = Map String (Int, [String])
type Seen = Set (String, [String], Int)

data Log = Log
  { opened :: [String]
  , steps :: Int
  , current :: String
  , activePressure :: Int
  , pressureReleased :: Int
  } deriving (Show, Eq, Ord)

findDistances :: Volcano -> Map String [(String, Int)]
findDistances volc = Map.fromList $ map ((,) <*> dists) $ Map.keys volc
  where
    dists :: String -> [(String, Int)]
    dists k = nubOrdOn fst
            $ filter (\(name, _) -> name /= k && fst (volc Map.! name) /= 0)
            $ go (Set.singleton k) [(k,0)]
    go :: Set String -> [(String, Int)] -> [(String, Int)]
    go seen xs = case concatMap (move seen) xs of
                   [] -> xs
                   new -> go (foldr (Set.insert . fst) seen new) (xs <> new)
    move :: Set String -> (String, Int) -> [(String, Int)]
    move seen (from, dist) =
      [ (to, succ dist)
      | to <- snd (volc Map.! from)
      , Set.notMember to seen
      ]

compute :: Int -> Volcano -> [String] -> Map String [(String, Int)] -> [Log]
compute n volc seen dists = dfs repr (`next` seen) (Log seen (-1) "AA" 0 0)
  where
    repr (Log opened steps current pressure total) = (current : opened, steps)
    next (Log opened ss current pressure total) seen =
      let press = fst (volc Map.! current)
       in case filter (\(name,d) -> name `notElem` opened) (dists Map.! current) of 
            [] -> guard (current `notElem` opened) *> 
              [Log (current : opened) (ss + 1) current (pressure + press) (total + pressure)]
            xs -> filter ((<= n) . steps)
                $ flip map xs $ \(name, dist) ->
                    Log (current : opened)
                        (ss + dist + 1)
                        name
                        (pressure + press)
                        (total + pressure + ((pressure + press) * dist))

value :: Int -> Log -> Int
value n (Log _ steps _ activePressure total) = total + (activePressure * (n - steps))

part2 :: Map String [(String, Int)] -> Volcano -> Int
part2 dists input = maximum 
                  . map (uncurry (+))
                  . findPart2 
                  . map ((,) . maximum . map fst <*> Set.delete "AA" . snd . head)
                  . groupOn snd 
                  . sortOn snd 
                  . map ((,) . value 26 <*> Set.fromList . opened) 
                  $ compute 27 input [] dists
  where
    findPart2 xs = [ (w,v) | ((v,vy):rest) <- tails xs, (w,wy) <- rest, Set.disjoint vy wy ]

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day16.in"
  let dists = findDistances input
  print . maximum . map (value 30) $ compute 30 input [] dists
  print $ part2 dists input

parseInput :: String -> Map String (Int, [String])
parseInput = Map.fromList . map f . lines
  where
    f xs = (name, (rate, map (filter (/= ',')) paths))
      where
        rate = head $ allNums xs
        (_:name:_:_:_:_:_:_:_:paths) = words xs

-- 1728
-- 2304
