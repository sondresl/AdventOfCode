module Advent.Coord where

import Linear

type Coord = V2 Int
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

invert :: V2 a -> V2 a
invert (V2 x y) = V2 y x
