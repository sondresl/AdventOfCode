module Day20 where

import Lib (linedNums)
import Control.Lens (view)
import Data.List.PointedList.Circular (PointedList)
import qualified Data.List.PointedList.Circular as PL

solve :: Int -> PointedList (Int, Int) -> PointedList (Int, Int)
solve mx = go 1
  where 
    go :: Int -> PointedList (Int, Int) -> PointedList (Int, Int)
    go ix xs | ix > mx = xs
    go ix (shiftToIndex ix -> xs) = 
      let (i, v) = view PL.focus xs
          toMove = if v < 0 then -((abs v) `mod` pred mx) else (v `mod` pred mx)
          Just oldRemoved = PL.deleteRight xs
          added = PL.insertLeft (i,v) $ PL.moveN toMove oldRemoved -- xs
       in go (ix + 1) added

    shiftToIndex :: Int -> PointedList (Int, Int) -> PointedList (Int, Int)
    shiftToIndex ix xs
      | (i, v) <- view PL.focus xs, i == ix = xs
      | otherwise = shiftToIndex ix (PL.moveN 1 xs)

score :: PointedList (Int, Int) -> Int
score xs = let Just st = PL.find 0 $ fmap snd xs
          in view PL.focus (PL.moveN 1000 st)
           + view PL.focus (PL.moveN 2000 st)
           + view PL.focus (PL.moveN 3000 st)

main :: IO ()
main = do
  input <- linedNums <$> readFile "../data/day20.in"
  let mx = length input
  let Just p1 = PL.fromList $ zip [1..] input
  print . score $ solve mx p1
  let Just p2 = PL.fromList . zip [1..] $ map (* 811589153) input
  print . score . (!! 10) $ iterate (solve mx) p2

-- 5962
-- 9862431387256

