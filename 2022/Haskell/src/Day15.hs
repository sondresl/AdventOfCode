module Day15 where

import Lib
import Linear hiding (trace)
import Advent.Coord
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List.Extra (firstJust, find)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec hiding (count)
import Data.Interval (Interval)
import qualified Data.Interval as I
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import qualified Data.ExtendedReal as E

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
     allPoints = foldr IS.insert IS.empty
               . map (I.singleton . view _x)
               $ filter ((==n) . view _y) 
               $ map location input <> map closest input

part2 :: [Beacon] -> Integer -> Integer -> Maybe Integer
part2 input upperBound y = ((y +) . (* 4000000)) <$> (f =<< line)
  where
    line = listToMaybe . IS.toList $ IS.difference interval (IS.unions $ map (getInterval y) input)
    interval = IS.singleton (E.Finite 0 I.<=..<= E.Finite upperBound)
    f iv = guard (I.width iv == 2) >> let E.Finite f = I.lowerBound iv in Just (f + 1)

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day15.in"
  print $ part1 2000000 input 
  print $ firstJust (part2 input 4000000) [0..4000000]

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
