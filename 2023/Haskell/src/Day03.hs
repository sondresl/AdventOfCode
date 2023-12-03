module Day03 where

import Lib (parseAsciiMap, neighbours, Point)
import Advent.Coord (left, right)
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map

data Schema = Number Int | Gear | Symbol
  deriving (Show, Eq)

findNumbers :: Map Point Schema -> Map Point [Int]
findNumbers mp = Map.fromListWith (<>) $ do
  (c, sc) <- filter (isNumber . snd) $ Map.assocs mp
  guard . not . any isNumber $ Map.lookup (left + c) mp
  let allCs = takeWhile (any isNumber . (`Map.lookup` mp)) $ iterate (+ right) c
      toNum = foldl (\acc n -> let Number num = mp Map.! n in acc * 10 + num) 0
  Just v <- pure $ find (any isSymbol . (`Map.lookup` mp)) $ concatMap neighbours allCs
  pure (v, [toNum allCs])
    where
      isSymbol = (`elem` [Gear, Symbol])
      isNumber (Number _) = True
      isNumber _ = False

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day03.in"
  let numberMap = findNumbers input
  print $ sum (sum <$> numberMap)
  let isGear x = let Just v = x `Map.lookup` input in v == Gear
  print . sum . map (product . snd) . filter ((&&) <$> isGear . fst <*> ((==2) . length . snd)) $ Map.assocs numberMap

parseInput :: String -> Map Point Schema
parseInput = parseAsciiMap f
  where f '*' = Just Gear
        f '.' = Nothing
        f x | isDigit x = Just (Number $ read $ pure x)
        f _ = Just Symbol

-- 522726
-- 81721933
