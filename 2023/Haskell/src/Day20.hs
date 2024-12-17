module Day20 where

import Lib
import Advent.Coord
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count)
import qualified Data.Sequence as Seq
import           Data.Sequence ( Seq, empty )
import qualified Data.Set as Set
import           Data.Set ( Set )
import Debug.Trace

data Signal = High | Low
  deriving (Show, Eq, Ord)

data Module = FlipFlop | Conjunction | Simple
  deriving (Show, Eq, Ord)

data ModuleStatus = FlipFlopStatus Bool
                  | ConjunctStatus (Map String Signal)
  deriving (Show, Eq, Ord)

type Modules = Map String (Module, [String])

startState :: Modules -> Map String ModuleStatus
startState input = flipFlops <> conjunctions input
  where
    flipFlops = Map.fromList . map (,FlipFlopStatus False) . Map.keys . Map.filter ((== FlipFlop) . fst) $ input

conjunctions :: Map String (Module, [String]) -> Map String ModuleStatus
conjunctions input = Map.fromList $ do
  name <- Map.keys $ Map.filter ((== Conjunction) . fst) input
  let inputs = map ((,Low) . fst) . filter ((name `elem`) . snd . snd) $ Map.assocs input
  pure (name, ConjunctStatus $ Map.fromList inputs)

moduleSearch ::
  Map String (Module, [String]) ->
  [(String, Signal, String)] -> -- Initial candidates
  Int -> -- How many times to press the button
  [(String, Signal, String)] -- All the visited 'areas'
moduleSearch input start cnt = go 1 (startState input) (Seq.fromList start)
  where
    go n state next
      | n > cnt = []
      | Seq.null next = go (succ n) state (Seq.fromList start)
      | otherwise = 
        let (c@(src, signal, dest) Seq.:<| cs) = next
            (moduleType, cands) = input Map.! dest
            (state', cands') = case moduleType of
                FlipFlop -> let FlipFlopStatus mode = state Map.! dest
                                (newState, sendTo) = case (mode, signal) of
                                           (_, High) -> (mode, [])
                                           (False, Low) -> (not mode, map (dest,High,) cands)
                                           (True, Low) -> (not mode, map (dest,Low,) cands)
                                state' = Map.insert dest (FlipFlopStatus newState) state
                             in (state', sendTo)
                Conjunction -> let ConjunctStatus modes = state Map.! dest
                                   modes' = Map.insert src signal modes
                                   state' = Map.insert dest (ConjunctStatus modes') state
                                in if all (== High) modes'
                                       then (state', map (dest,Low,) cands)
                                       else (state', map (dest,High,) cands)
                Simple -> (state, map (dest,signal,) cands)
         in c : go n state' (cs Seq.>< Seq.fromList cands')

findCycles :: Set String -> Int -> [(String, Signal, String)] -> [(String, Int)]
findCycles targets buttonPresses [] = []
findCycles targets buttonPresses xs | Set.null targets = []
findCycles targets buttonPresses (("button", signal, dest):xs) = findCycles targets (succ buttonPresses) xs
findCycles targets buttonPresses ((src, High, dest):xs) 
  | src `elem` targets = (src, buttonPresses) : findCycles (Set.delete src targets) buttonPresses xs
findCycles targets buttonPresses (_:xs) = findCycles targets buttonPresses xs

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day20.in"
  let res = moduleSearch input [("button", Low, "broadcaster")] 1000
  print $ uncurry (*) $ foldl (\(lo, hi) (_,s,_) -> if s == High then (lo, succ hi) else (succ lo, hi)) (0,0) res
  let targets = Set.fromList ["hh", "fh", "lk", "fn"]
      ps = findCycles targets 0 $ moduleSearch input [("button", Low, "broadcaster")] 10000
  print $ foldl1 lcm $ map snd ps

parseInput :: String -> Modules
parseInput = Map.fromList 
           . (("rx", (Simple, [])):) 
           . (("output", (Simple, [])):) 
           . (("button", (Simple, ["broadcaster"])):) 
           . map (f . tuple . splitOn " -> ") 
           . lines
  where
    f ('%':sender, receivers) = (sender, (FlipFlop, splitOn ", " receivers))
    f ('&':sender, receivers) = (sender, (Conjunction, splitOn ", " receivers))
    f (sender, receivers) = (sender, (Simple, splitOn ", " receivers))

-- 1020211150
-- 238815727638557
