module Day15 where

import Lib
import Linear hiding (trace)
import Advent.Coord
import Control.Lens (view)
import Control.Monad (guard, (<=<))
import Data.Maybe (listToMaybe)
import Data.List.Extra (firstJust, find)
import Text.ParserCombinators.Parsec (string, parse, try, (<|>), digit, many1, char)
import Data.Interval (Interval)
import qualified Data.Interval as I
import qualified Data.Map as Map
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import qualified Data.ExtendedReal as E
import Data.SBV

data Beacon = Beacon
  { location :: V2 Integer
  , radius :: Integer
  , closest :: V2 Integer
  } deriving (Show, Eq, Ord)

getInterval :: Integer -> Beacon -> IntervalSet Integer
getInterval n (Beacon pos rad other) = 
  let distToN (V2 _ y) = fromIntegral $ abs (y - n)
      radLeft = rad - distToN pos
      x = fromIntegral $ view _x pos
      interval = case radLeft `compare` 0 of 
                   EQ -> IS.singleton (I.singleton x)
                   GT -> IS.singleton (E.Finite (x - radLeft) I.<=..<= E.Finite (x + radLeft))
                   LT -> IS.empty
      beacs = if other == V2 x n then IS.singleton (I.singleton x) else IS.empty
   in IS.difference interval beacs

part1 :: Integer -> [Beacon] -> Integer
part1 n input = sum . map I.width . IS.toList $ IS.difference pointsOnLine allPoints
  where
     pointsOnLine = IS.unions $ map (getInterval n) input
     allPoints = foldr (IS.insert . I.singleton . view _x) IS.empty
               $ filter ((==n) . view _y) 
               $ map location input <> map closest input

part2 :: [Beacon] -> SymbolicT IO ()
part2 beacons = do
  [x,y] <- sIntegers ["x", "y"]
  constrain $ x .> 0 .&& x .< 4000000
  constrain $ y .> 0 .&& y .< 4000000
  constrain $ sAll (run (x,y)) beacons
    where 
      run (x, y) (Beacon (V2 x' y') rad _) =
        abs (x - literal x') + abs (y - literal y') .> literal rad

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day15.in"
  print $ part1 2000000 input 
  satRes <- sat (part2 input)
  let Just x = getModelValue "x" satRes :: Maybe Integer
      Just y = getModelValue "y" satRes :: Maybe Integer
  print $ x * 4000000 + y

parseInput :: String -> [Beacon]
parseInput = either (error . show) id . traverse (parse p "") . lines
  where
    num = try neg <|> (read <$> many1 digit)
    neg = negate . read <$> (char '-' >> many1 digit)
    p = do
      (x,y) <- (,) <$> (string "Sensor at x=" *> num <* string ", y=") <*> num
      (a,b) <- (,) <$> (string ": closest beacon is at x=" *> num <* string ", y=") <*> num
      let beacon = V2 x y
          other = V2 a b
      pure $ Beacon beacon (mannDist other beacon) other

-- 5832528
-- 13360899249595
