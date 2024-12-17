{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Lens
import Data.Array (accumArray, elems)
import Data.Bool (bool)
import Data.Foldable ( Foldable(foldl', toList) )
import Data.List.Extra (transpose, tails, inits, splitOn, chunksOf, minimumBy, maximumBy)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Max (Max, getMax), Min (Min, getMin))
import Linear ( V2(..) )
import qualified Data.Set as Set
import           Data.Set ( Set )
import Control.Monad (foldM)
import qualified Data.List.NonEmpty as NE

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

rotate :: Int -> [a] -> [a]
rotate = drop <> take

zipWithTail :: [a] -> [(a, a)]
zipWithTail = zip <*> tail

zipWithTail' :: [a] -> [(a, a)]
zipWithTail' = zip <*> rotate 1

iterateN :: Int -> (a -> a) -> a -> a
iterateN n f start = iterate f start !! n

-- More effective than firstRepeat, it does only need to hold two versions
-- of the 'a' in memory at one time.
fixedPoint :: Eq a => (a -> a) -> a -> Maybe a
fixedPoint = fixedPointOn id

fixedPointOn :: Eq b => (a -> b) -> (a -> a) -> a -> Maybe a
fixedPointOn project f x = either Just (const Nothing) . foldM go x $ iterate f (f x)
  where
    go !x !x'
     | project x == project x' = Left x
     | otherwise = Right x'

firstRepeat :: (Foldable t, Ord a) => t a -> Maybe a
firstRepeat = either Just (const Nothing) . foldM f Set.empty
 where
  f seen x
    | Set.member x seen = Left x
    | otherwise = Right $ Set.insert x seen

firstRepeatOn :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
firstRepeatOn project = either Just (const Nothing) . foldM f Set.empty
 where
   f seen x = let var = project x
               in if Set.member var seen
                     then Left x
                     else Right $ Set.insert var seen

bfs ::
  Ord a =>
  [a] -> -- Initial candidates
  (a -> [a]) -> -- Generate new candidates from current
  [a] -- All the visited 'areas'
bfs start fn = go Set.empty start
  where
    go _ [] = []
    go seen (c : cs) =
      let cands = filter (not . (`Set.member` seen)) $ fn c
          seen' = Set.union seen $ Set.fromList cands
       in c : go (Set.insert c seen') (cs ++ cands)

-- | Find the lowest value where the predicate is satisfied within the
-- given bounds.
binaryMinSearch
    :: (Int -> Bool)
    -> Int                  -- ^ min
    -> Int                  -- ^ max
    -> Maybe Int
binaryMinSearch p = go
  where
    go !x !y
        | x == mid || y == mid = Just (x + 1)
        | p mid                = go x mid
        | otherwise            = go mid y
      where
        mid = ((y - x) `div` 2) + x

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = Map.fromListWith (+) . map (,1) . toList

-- | Useful functions for displaying some collection of points in 2D
findBounds ::
  -- | The points in 2D space
  [(Int, Int)] ->
  -- | V2 (V2 minX minY) (V2 maxX maxY)
  (Int, Int, Int, Int)
findBounds cs = (getMin minX, getMin minY, getMax maxX, getMax maxY)
 where
  (minX, minY, maxX, maxY) = foldMap f cs
  f (x, y) = (Min x, Min y, Max x, Max y)

type Point = V2 Int

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map Point a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton

asciiGrid :: IndexedFold Point String Char
asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

display ::
  (Foldable t) =>
  -- | Projection function
  (a -> ((Int, Int), Bool)) ->
  -- | the bounds of the points
  (Int, Int, Int, Int) ->
  -- | foldable of coordinates
  t a ->
  String
display project (minX, minY, maxX, maxY) =
  unlines
    . chunksOf (maxX - minX + 1)
    . map (bool '░' '▓')
    . elems
    . accumArray (||) False ((minY, minX), (maxY, maxX))
    . map project
    . toList

-- | All eight surrounding neighbours
neighbours :: Point -> [Point]
neighbours p0 =
  [ p
  | x <- [-1 .. 1]
  , y <- [-1 .. 1]
  , p <- [p0 + V2 x y]
  , p /= p0
  ]

-- | Neighbours left, right, above and below
neighbours4 :: Point -> [Point]
neighbours4 p = (p +) <$> [V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1)]

mannDist :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
mannDist x y = sum . abs $ x - y

-- | Various simple parser
linedNums :: String -> [Int]
linedNums = map read . lines . filter (/= '+')

commaNums :: String -> [Int]
commaNums = map read . splitOn ","

-- Fold
unionFind :: Ord a => (a -> Set a -> Bool) -> [a] -> Set (Set a)
unionFind f = foldl' go Set.empty
  where
    go set x = let (same, diff) = Set.partition (f x) set
                in Set.insert (Set.unions same <> Set.singleton x) diff

-- Some directional stuff, for moving in a certain facing
data Dir = North | South | East | West
  deriving (Ord, Eq, Show, Bounded, Enum)

-- North is like moving forward, so does not change direction
instance Semigroup Dir where
  (<>) North = id
  (<>) East  = \case North -> East
                     East  -> South
                     South -> West
                     West  -> North
  (<>) South = \case North -> South
                     East  -> West
                     South -> North
                     West  -> East
  (<>) West  = \case North -> West
                     East  -> North
                     South -> East
                     West  -> South

instance Monoid Dir where
  mempty = North

dirPoint :: Dir -> Point
dirPoint North = V2 0 1
dirPoint South = V2 0 (-1)
dirPoint West = V2 (-1) 0
dirPoint East = V2 1 0


-- | Get the key-value pair corresponding to the maximum value in the map
maximumVal :: Ord b => Map a b -> Maybe (a, b)
maximumVal = maximumValBy compare

-- | Get the key-value pair corresponding to the maximum value in the map,
-- with a custom comparing function.
--
-- > 'maximumVal' == 'maximumValBy' 'compare'
maximumValBy :: (b -> b -> Ordering) -> Map a b -> Maybe (a, b)
maximumValBy c = fmap (maximumBy (c `on` snd))
               . NE.nonEmpty
               . Map.toList

-- | Get the key-value pair corresponding to the minimum value in the map,
-- with a custom comparing function.
--
-- > 'minimumVal' == 'minimumValBy' 'compare'
minimumValBy :: (b -> b -> Ordering) -> Map a b -> Maybe (a, b)
minimumValBy c = fmap (minimumBy (c `on` snd))
               . NE.nonEmpty
               . Map.toList

-- | Get the key-value pair corresponding to the minimum value in the map
minimumVal :: Ord b => Map a b -> Maybe (a, b)
minimumVal = minimumValBy compare
