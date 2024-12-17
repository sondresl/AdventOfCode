module Day22 where

import Linear (V3(..))
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec (many1, digit, letter, string, space, char, parse, (<|>))

type Bound = (V3 Integer, V3 Integer)
data Cube = On | Off deriving (Show, Eq, Ord)

cubeIntersection :: Bound -> Bound -> Maybe Bound
cubeIntersection (V3 x1 y1 z1, V3 x2 y2 z2) (V3 x3 y3 z3, V3 x4 y4 z4) =
  let x1' = max x1 x3; x2' = min x2 x4 
      y1' = max y1 y3; y2' = min y2 y4
      z1' = max z1 z3; z2' = min z2 z4
      res = (V3 x1' y1' z1', V3 x2' y2' z2')
   in if x1' <= x2' && y1' <= y2' && z1' <= z2'
         then Just res
         else Nothing

solve :: [(Cube, Bound)] -> Map Bound Integer
solve = go Map.empty
  where
    go :: Map Bound Integer -> [(Cube, Bound)] -> Map Bound Integer
    go cnt [] = cnt
    go cnt ((cmd, bs):rest) = 
      let seen = Map.unionWith (+) cnt $ allIntersections bs cnt
          new = if cmd == On 
                   then Map.insertWith (+) bs 1 seen
                   else seen
       in go new rest

allIntersections :: Bound -> Map Bound Integer -> Map Bound Integer
allIntersections bs cnt = 
  Map.unionsWith (+) . map (uncurry Map.singleton) . mapMaybe (\(k, v) -> (,-v) <$> cubeIntersection bs k) 
     $ Map.toList cnt

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day22.in"
  let vSize (V3 x1 y1 z1, V3 x2 y2 z2) v = (1 + x2 - x1) * (1 + y2 - y1) * (1 + z2 - z1) * v
      total = sum . map (uncurry vSize) . Map.toList
      res = solve input
  print $ total . Map.map negate $ allIntersections (V3 (-50) (-50) (-50), V3 50 50 50) res
  print $ total res

parseInput :: String -> [(Cube, Bound)]
parseInput = either (error . show) id . traverse (parse p "") . lines
  where
    neg = negate <$> (char '-' *> num)
    pos = read @Integer <$> many1 digit
    num = neg <|> pos
    p = do
      cmd <- (\x -> if x == "on" then On else Off) <$> many1 letter <* space
      (x1,x2) <- (,) <$> (string "x=" *> num <* string "..") <*> num <* char ','
      (y1,y2) <- (,) <$> (string "y=" *> num <* string "..") <*> num <* char ','
      (z1,z2) <- (,) <$> (string "z=" *> num <* string "..") <*> num
      pure (cmd, (V3 x1 y1 z1, V3 x2 y2 z2))

-- 642125
-- 1235164413198198
