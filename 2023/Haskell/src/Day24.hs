module Day24 where

import Lib (allNums, combinations, tuple)
import Data.Maybe (mapMaybe)
import Control.Monad (guard)
import Data.List.Extra (splitOn)
import Linear (V3(V3))
import Data.SBV hiding (solve)
import Data.Foldable

type Hail a = (V3 a, V3 a)

-- https://paulbourke.net/geometry/pointlineplane/javascript.txt
part1 :: [Hail AlgReal] -> Int
part1 input = length . mapMaybe (uncurry inter . tuple) $ combinations 2 input
  where
    inter (V3 x1 y1 _, V3 dx1 dy1 _) (V3 x3 y3 _, V3 dx3 dy3 _) = guard valid *> Just (px, py)
      where
        lb = 200000000000000
        ub = 400000000000000
        (x2, y2) = (x1 + dx1, y1 + dy1)
        (x4, y4) = (x3 + dx3, y3 + dy3)
        denom = (x1-x2)*(y3-y4)-(y1-y2)*(x3-x4)
        a = ((x4-x3)*(y1-y3)-(y4-y3)*(x1-x3)) / denom
        b = ((x2-x1)*(y1-y3)-(y2-y1)*(x1-x3)) / denom
        px = x1 + a*(x2-x1)
        py = y1 + a*(y2-y1)
        valid = denom /= 0 && inBounds && forwardx1x && forwardx1y && forwardx3x && forwardx3y
        inBounds = lb <= px && px <= ub && lb <= py && py <= ub
        forwardx1x = (dx1 >= 0 && px >= x1) || (dx1 <= 0 && px <= x1)
        forwardx1y = (dy1 >= 0 && py >= y1) || (dy1 <= 0 && py <= y1)
        forwardx3x = (dx3 >= 0 && px >= x3) || (dx3 <= 0 && px <= x3)
        forwardx3y = (dy3 >= 0 && py >= y3) || (dy3 <= 0 && py <= y3)


part2 :: [Hail AlgReal] -> IO AlgReal
part2 input = do
  model <- sat solve
  let Just (x, y, z) = (,,) <$> getModelValue "x" model 
                            <*> getModelValue "y" model
                            <*> getModelValue "z" model
  pure $ x + y + z
    where 
      solve = do
        [x,y,z,dx,dy,dz] <- sReals ["x","y","z","dx","dy","dz"]
        traverse_ (f (x,y,z,dx,dy,dz)) (take 3 input)
          where
            f (x,y,z,dx,dy,dz) (V3 px py pz, V3 dpx dpy dpz) = do
              t <- sReal ("t" <> show px)
              constrain $ literal px + literal dpx * t .== x + dx * t
              constrain $ literal py + literal dpy * t .== y + dy * t
              constrain $ literal pz + literal dpz * t .== z + dz * t

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day24.in"
  print $ part1 input
  print =<< part2 input

parseInput :: Num a => String -> [Hail a]
parseInput = map (f . map allNums . splitOn " @ ") . lines
  where f [map fromIntegral -> [x,y,z],map fromIntegral -> [dx,dy,dz]] = (V3 x y z, V3 dx dy dz)

-- 16727
-- 606772018765659
