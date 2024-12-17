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

invertSignal High = Low
invertSignal Low = High

data Mode = Off | On
  deriving (Show, Eq, Ord)

flipMode Off = On
flipMode On = Off

data Module = FlipFlop | Conjunction | Simple
  deriving (Show, Eq, Ord)

data ModuleStatus = FlipFlopStatus Mode
                  | ConjunctStatus (Map String Signal)
  deriving (Show, Eq, Ord)

type Modules = Map String (Module, [String])
 
part1 input = undefined

part2 input = undefined

startState :: Modules -> Map String ModuleStatus
startState input = flipFlops <> conjunctions input
  where
    flipFlops = Map.fromList . map (,FlipFlopStatus Off) . Map.keys . Map.filter ((== FlipFlop) . fst) $ input

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
                                           (Off, Low) -> (flipMode mode, map (dest,High,) cands)
                                           (On, Low) -> (flipMode mode, map (dest,Low,) cands)
                                state' = Map.insert dest (FlipFlopStatus newState) state
                             in (state', sendTo)
                Conjunction -> let ConjunctStatus modes = state Map.! dest
                                   modes' = Map.insert src signal modes
                                   state' = if dest `elem` ["nc", "fh", "fn", "hh", "lk"]
                                     then Map.insert dest (ConjunctStatus modes') state
                                     else Map.insert dest (ConjunctStatus modes') state
                                in if all (== High) modes'
                                       then (state', map (dest,Low,) cands)
                                       else (state', map (dest,High,) cands)
                Simple -> (state, map (dest,signal,) cands)
         in c : go n state' (cs Seq.>< Seq.fromList cands')

-- findCycles :: Map String ModuleStatus -> [(String, Signal, String)] ->
findCycles state buttonPresses [] = []
findCycles state buttonPresses (("button", signal, dest):xs) = findCycles state (succ buttonPresses) xs
findCycles state buttonPresses ((src, signal, dest):xs)
  | Map.member dest state = let ConjunctStatus modes = state Map.! dest
                                modes' = Map.insert src signal modes
                                state' = Map.insert dest (ConjunctStatus modes') state
                             in if dest `elem` ["hh", "fh", "lk", "fn"] && all (==High) modes'
                                    then (dest, buttonPresses) : findCycles state' buttonPresses xs
                                    else findCycles state' buttonPresses xs
  | otherwise = findCycles state buttonPresses xs
-- hh fh lk fn

main :: IO ()
main = do

  let run str input = do
        putStrLn str
        let res = moduleSearch input [("button", Low, "broadcaster")] 1000
        print $ foldl (\(lo, hi) (_,s,_) -> if s == High then (lo, succ hi) else (succ lo, hi)) (0,0) res
        print $ uncurry (*) $ foldl (\(lo, hi) (_,s,_) -> if s == High then (lo, succ hi) else (succ lo, hi)) (0,0) res
        print $ [ () | (_,Low,"rx") <- res ]
        pprint $ findCycles (conjunctions input) 0 res

        -- print $ part1 input
        -- print $ part2 input
    
  -- run "\nTest:\n\n" $ parseInput testInput
  -- run "\nTest 2:\n\n" $ parseInput testInput2

  input <- parseInput <$> readFile "../data/day20.in"
  run "\nActual:\n\n" input

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

-- parseInput = either (error . show) id . traverse (parse p "") . lines
--   where
--     p = undefined

testInput = [r|broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
|]

testInput2 = [r|broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
|]
