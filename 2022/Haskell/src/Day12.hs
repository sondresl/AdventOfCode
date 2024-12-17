module Day12 where

import Lib (parseAsciiMap, bfs, neighbours4)
import Linear (V2(..))
import Data.List.Extra (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type GPS = Map (V2 Int) Char
type Pos = (V2 Int, Int) -- Position, length to that position

move :: GPS -> (GPS -> [Pos]) -> Maybe Int
move input start = listToMaybe [ v | (k, v) <- bfs (start input) ns fst, input Map.! k == 'E']
  where 
    ns (pos, dist) = map (,dist + 1) 
                   . filter ((&&) <$> (`Map.member` input) <*> ((<= succ next) . convert . (input Map.!))) 
                   $ neighbours4 pos
      where next = convert $ input Map.! pos
            convert 'S' = 'a'
            convert 'E' = 'z'
            convert c = c

main :: IO ()
main = do
  input <- parseAsciiMap Just <$> readFile "../data/day12.in"
  let start str = map (,0) . Map.keys . Map.filter (`elem` str)
      run = move input
  print $ run (start "S")
  print $ run (start "Sa")

-- 462
-- 451
