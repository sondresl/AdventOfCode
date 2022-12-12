module Day12 where

import Lib (parseAsciiMap, bfs, neighbours4)
import Linear (V2(..))
import Data.List.Extra (find)
import Data.Map (Map)
import qualified Data.Map as Map

type GPS = Map (V2 Int) Char
type Pos = (V2 Int, Int) -- Position, length to that position

move :: GPS -> (GPS -> [Pos]) -> [Pos]
move input start = bfs (start input) (ns input) fst
  where 
    ns mp (pos, dist) = map (,dist + 1) . filter (\p -> p `Map.member` mp && (convert (mp Map.! p) <= succ next)) $ neighbours4 pos
      where next = convert $ mp Map.! pos
            convert 'S' = 'a'
            convert 'E' = 'z'
            convert c = c

main :: IO ()
main = do
  input <- parseAsciiMap Just <$> readFile "../data/day12.in"
  let start str = map (,0) . Map.keys . Map.filter (`elem` str)
      run = fmap snd . find ((=='E') . (input Map.!) . fst) . move input
  print $ run (start "S")
  print $ run (start "Sa")

-- 462
-- 451
