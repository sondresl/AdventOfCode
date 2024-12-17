module Advent.Coord where

import Linear
import Control.Lens (over)

type Coord = V2 Int
type Dir = V2 Int

origin, up, down, left, right :: (Traversable t, Applicative t, Num a, Eq (t a), R2 t) =>
  t a 
origin = pure 0
up     = over _y (+1)         $ pure 0
down   = over _y (subtract 1) $ pure 0
left   = over _x (+1)         $ pure 0
right  = over _x (subtract 1) $ pure 0

above, below :: (Traversable t, Applicative t, Num a, Eq (t a), R3 t) =>
  t a 
above = over _z (+1)         $ pure 0
below = over _z (subtract 1) $ pure 0

turnAround, turnLeft, turnRight :: V2 Int -> V2 Int
turnLeft = perp
turnRight = perp . perp . perp
turnAround = negate

invert :: V2 a -> V2 a
invert (V2 x y) = V2 y x
