module Main where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Linear
import Data.Maybe (mapMaybe)
import Data.List (find)

data Walker = Walker
  { pos :: Point
  , dir :: Dir
  } deriving Show

type Point = V2 Int
type Dir = V2 Int

origin, up, down, left, right :: V2 Int
origin= V2 0 0
up = V2 0 1
down = V2 0 (-1)
left = V2 (-1) 0
right = V2 1 0

turnAround, turnLeft, turnRight :: V2 Int -> V2 Int
turnLeft = perp
turnRight = perp . perp . perp
turnAround = negate

mannDist :: Point -> Int
mannDist = f (V2 0 0)
  where f x = sum . abs . (`subtract` x)

neighbours :: Point -> [Point]
neighbours p0 = fmap (+ p0) . tail $ V2 <$> [0, 1, -1] <*> [0, 1, -1]

oneLeft :: Walker -> Point
oneLeft (Walker pos dir) = pos + turnLeft dir

forward :: Walker -> Map Point Int -> Walker
forward w@(Walker pos dir) xs =
  let left' = oneLeft w
   in if Map.member left' xs 
         then Walker (pos + dir) dir
         else Walker left' (turnLeft dir)

part1 :: (Int -> Map Point Int -> Int) -> Int -> Int
part1 newVal n = mannDist . last $ evalState (traverse f [2..n]) (Walker (V2 0 0) down, Map.singleton (V2 0 0) 0)
  where
    f :: Int -> State (Walker, Map Point Int) Point
    f n = do
      (walker, xs) <- get
      let new@(Walker pos _) = forward walker xs
          value = newVal n xs
      put $ (new, Map.insert pos value xs)
      pure pos

part2 :: (Int -> [Int] -> Int) -> Int -> Int
part2 newVal n = head . dropWhile (<= n) $ evalState (traverse f [2..]) (Walker (V2 0 0) down, Map.singleton (V2 0 0) 1)
  where
    f :: Int -> State (Walker, Map Point Int) Int
    f n = do
      (walker, xs) <- get
      let new@(Walker pos _) = forward walker xs
          value = newVal n (mapMaybe (`Map.lookup` xs) (neighbours pos))
      put $ (new, Map.insert pos value xs)
      pure value

main :: IO ()
main = do
  let input = 289326
  print $ part1 const input
  print $ part2 ((sum .) . flip const) input

-- 419
-- 295229
