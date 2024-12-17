module Day13 where

import Control.Lens
import Lib (count, neighbours4)
import Data.List.Extra
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear hiding (trace)
import Data.Digits (digits)

type Point = V2 Int

open :: Int -> Point -> Bool
open salt (V2 x y) = 
  let val = (x * x) + (3 * x) + (2 * x * y) + y + (y * y)
      withSalt = val + salt
      ones = count (== 1 ) $ digits 2 withSalt
   in x >= 0 && y >= 0 && even ones

bfs
  :: [Point]          -- Initial candidates
  -> (Point -> Bool)  -- If this candidate point is a wall/space
  -> [(Point, Int)] -- Steps taken
bfs start fn = go Set.empty $ map (,0) start
  where
    go _ [] = []
    go seen ((c, steps) : cs) = 
      let cands = filter (not . (`Set.member` seen)) . filter fn $ neighbours4 c
          seen' = Set.union seen $ Set.fromList cands
       in (c, steps) : go (Set.insert c seen') (cs ++ map (, steps + 1) cands)

part1 :: (Point -> Bool) -> Maybe Int
part1 = lookup (V2 31 39) . bfs [V2 1 1]

part2 :: (Point -> Bool) -> Int
part2 = length . takeWhile ((<= 50) . snd) . bfs [V2 1 1]

main :: IO ()
main = do
  let run str fn = do
        putStrLn str
        print $ part1 fn
        print $ part2 fn

  run "\nActual\n" (open 1352)

-- Just 90
-- 135
