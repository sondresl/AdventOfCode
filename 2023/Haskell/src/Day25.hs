module Day25 where

import Lib
import Advent.Coord
import Advent.Search
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count)
import Data.Graph
import Data.Tree

part1 input = undefined

-- part2 input = let [a,b] = components $ mkGraph mp in length a * length b
part2 input = let [a,b] = components graph in length a * length b
  where
    res = test input
    mp = foldl (\acc (from, to) -> Map.adjust (delete to) from $ Map.adjust (delete from) to acc) input res
    (graph,_,_) = mkGraph mp

mkGraph :: Map String [String] -> (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
mkGraph mp = graphFromEdges $ do
  k <- Map.keys mp
  let to = mp Map.! k
  pure (k, k, to)

test mp = nub $ do
  k <- Map.keys mp
  let ts = mp Map.! k
  (t, rest) <- select ts
  -- BFS To the other neibhgours of k?
  let Just (d,res) = find (\(i,f) -> f `elem` rest) $ bfs [(0,t)] (\(i,f) -> map (i+1,) . filter (/= k) $ mp Map.! f)
  guard $ d > 6
  pure (min k t, max k t)


main :: IO ()
main = do

  let run str input = do
        putStrLn str
        print $ part2 input

        -- print $ part1 input
        -- print $ part2 input
    
  -- run "\nTest:\n\n" $ parseInput testInput

  input <- parseInput <$> readFile "../data/day25.in"
  run "\nActual:\n\n" input

parseInput = Map.unionsWith (<>) . map (f . splitOn ": ") . lines
  where 
    f [from, words -> to] = Map.unionsWith (<>) $ do
      t <- to
      pure $ Map.singleton t [from] <> Map.singleton from [t]

-- parseInput = either (error . show) id . traverse (parse p "") . lines
--   where
--     p = undefined

testInput = [r|jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
|]
