module Day15 where

import Lib (allNums, mannDist)
import Data.SBV 
 (sat, sIntegers, sInteger, getModelValue, constrain, (.>), (.&&), (.<), sAll, literal)
import Linear (V2(..))
import Text.ParserCombinators.Parsec (string, parse, try, (<|>), digit, many1, char)
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import Data.Interval ((<=..<=))
import qualified Data.Interval as I
import Data.ExtendedReal (Extended(Finite))

data Beacon = Beacon
  { location :: V2 Integer
  , radius :: Integer
  , closest :: V2 Integer
  } deriving (Show, Eq, Ord)

-- width is not length; add 1 to get the number of `elements`
part1 :: Integer -> [Beacon] -> Integer
part1 n input = subtract bs . sum . map ((+1) . I.width) . IS.toList . IS.fromList $ map getInterval input
  where
    bs = fromIntegral $ length [ 1 | (Beacon _ _ (V2 _ y)) <- input, y == n ]
    getInterval :: Beacon -> I.Interval Integer
    getInterval (Beacon (V2 (fromIntegral -> x) y) rad other) = 
      let radLeft = rad - fromIntegral (abs $ y - n)
       in case radLeft `compare` 0 of 
            EQ -> I.singleton x
            GT -> Finite (x - radLeft) <=..<= Finite (x + radLeft)
            LT -> I.empty

part2 :: [Beacon] -> IO Integer
part2 beacons = do
  model <- sat f
  let Just (x, y) = (,) <$> getModelValue "x" model <*> getModelValue "y" model
  pure $ x * 4000000 + y
    where 
      f = do 
        [x,y] <- sIntegers ["x", "y"]
        constrain $ x .> 0 .&& x .< 4000000
        constrain $ y .> 0 .&& y .< 4000000
        constrain $ sAll (run (x,y)) beacons
      run (x, y) (Beacon (V2 x' y') rad _) =
          abs (x - literal x') + abs (y - literal y') .> literal rad

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day15.in"
  print $ part1 2000000 input 
  print =<< part2 input

parseInput :: String -> [Beacon]
parseInput = map (f . allNums) . lines
  where
    f [sx,sy,bx,by] = Beacon beacon (mannDist other beacon) other
      where beacon = V2 sx sy
            other = V2 bx by

-- 5832528
-- 13360899249595
