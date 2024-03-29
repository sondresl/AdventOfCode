{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Comonad.Store (ComonadStore(experiment))
import Control.Lens
import Control.Monad ((<=<), guard, foldM)
import Data.Array (accumArray, elems)
import Data.Bool (bool)
import Data.Foldable ( Foldable(foldl', toList) )
import Data.List.Extra (transpose, tails, inits, splitOn, chunksOf, minimumBy, maximumBy)
import Data.Semigroup (Max (Max, getMax), Min (Min, getMin))
import Linear ( V2(..) )
import Data.Function ( on )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set ( Set )
import Data.Maybe (fromJust, isJust, listToMaybe)
import qualified Math.NumberTheory.Primes as Math

isPrime :: Integer -> Bool
isPrime = isJust . Math.isPrime

count :: Foldable t => (a -> Bool) -> t a -> Int
count p = length . filter p . toList

-- To get _all_ diagonals, do this once on the list and on the reversed list
diagonals :: [[a]] -> [[a]]
diagonals xs = lower ++ upper
  where
    upper = transpose $ zipWith drop [0..] xs
    lower = reverse $ transpose $ map reverse $ zipWith take [0..] xs

rotate :: Int -> [a] -> [a]
rotate = drop <> take

zipWithTail :: [a] -> [(a, a)]
zipWithTail = zip <*> tail

zipWithTail' :: [a] -> [(a, a)]
zipWithTail' = zip <*> rotate 1

-- Every combination of selecting one element
-- and removing it from the list
select :: [a] -> [(a, [a])]
select xs = do
  (is, t : ts) <- zip (inits xs) (tails xs)
  pure (t, is ++ ts)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = []
combinations 1 xs = map pure xs
combinations n xs = do
  t:ts <- tails xs
  cs <- combinations (pred n) ts
  pure $ t : cs

safeHead :: [a] -> Maybe a
safeHead = listToMaybe

tuple :: [a] -> (a, a)
tuple [a, b] = (a, b)

tuple3 :: [a] -> (a, a, a)
tuple3 [a, b, c] = (a, b, c)

-- Functions that test for repetition

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
firstRepeat = firstRepeatOn id

firstRepeatOn :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
firstRepeatOn project = either Just (const Nothing) . foldM f Set.empty
 where
   f seen x = let var = project x
               in if Set.member var seen
                     then Left x
                     else Right $ Set.insert var seen

-- Given tuples of tuples of 'time' and some period, when do they match up again
-- and what is the new period
sync :: (Int, Int) -> (Int, Int) -> (Int, Int)
sync (time, period) (time', period') =
  let candidates = dropWhile (< time) $ iterate (+ period') time'
      first = head $ dropWhile ((/= 0) . (`mod` period) . subtract time) candidates
   in (first, lcm period period')

--

-- Perturbations of a list, and max/min (key,value) from maps
-- based on the value
--
-- Source: https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Common.hs
--
perturbations
    :: Traversable f
    => (a -> [a])
    -> f a
    -> [f a]
perturbations = perturbationsBy traverse

perturbationsBy
    :: Conjoined p
    => Over p (Bazaar p a a) s t a a
    -> (a -> [a])
    -> s
    -> [t]
perturbationsBy p f = experiment f <=< holesOf p

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
---------------

-- Example shift-function, for a set
-- (\sh lo x -> S.map (+ (sh * lo)) x)
--
skipLoop :: Ord a => (a -> (Int, a)) -> (Int -> Int -> a -> a) -> Int -> [a] -> a
skipLoop normalize shift n xs = (!! extra) . map (shift loopShift looped) . drop loopN $ xs
  where
    (loopN, loopSize, loopShift) = findLoop normalize xs
    (looped, extra) = (n - loopN) `divMod` loopSize

findLoop :: Ord a => (a -> (Int, a)) -> [a] -> (Int, Int, Int) -- first loop, size of loop, shift
findLoop normalize (x:xs) = go (Map.singleton x (0, 0)) 1 xs
  where
    go !seen !i (w:ws) = case Map.lookup w'Norm seen of
                       Nothing -> go (Map.insert w'Norm (mn, i) seen) (i + 1) ws
                       Just (seenMn, seenI) -> (seenI, i - seenI, mn - seenMn)
      where
        (mn, w'Norm) = normalize w


-- Iterate a function n times
goN :: Int -> (a -> a) -> a -> a
goN n f = (!! n) . iterate f

-- Iterate a function until it returns Nothing.
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : case f x of
                         Nothing -> []
                         Just v -> iterateMaybe f v

toMapIndexed :: [a] -> Map Int a
toMapIndexed = Map.fromList . zip [0..]

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

dijkstra :: Map Int [(Int, Int)] -> Int -> Map Int Int
dijkstra graph start = go seen Map.empty
  where
    seen = Map.insert start (Just 0) . fmap (const Nothing) $ graph
    go waiting result
      | Map.null waiting = result
      | otherwise = 
        let next@(node, Just value) = 
              minimumBy (compare `on` (fromJust . snd)) $ filter (isJust . snd) $ Map.toList waiting
            targets = filter ((`elem` Map.keys waiting) . fst) $ graph Map.! fst next
            add weight Nothing = Just $ weight + value
            add weight (Just y) = Just $ min y (weight + value)
            new = foldr (\(k, v) acc -> Map.adjust (add v) k acc) waiting targets
         in go (Map.delete node new) (Map.insert node value result)

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

pointToTuple :: Point -> (Int, Int)
pointToTuple (V2 x y) = (x, y)

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
-- Will generate neighbours for V2, V3, V4 +++
neighbours ::
    (Traversable t, Applicative t, Num a, Eq (t a)) =>
    t a ->
    [t a]
neighbours p = do
    n <- tail $ sequenceA (pure [0, 1, -1])
    pure $ (+) <$> p <*> n

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

-- Inefficient version of unionFind
-- Find the disjoint unions of a list by some f,
-- that determines if an element should be in a set of elements or not.
unionFind :: Ord a => (a -> Set a -> Bool) -> [a] -> Set (Set a)
unionFind f = foldl' go Set.empty
  where
    go set x = let (same, diff) = Set.partition (f x) set
                in Set.insert (Set.unions same <> Set.singleton x) diff

conway :: Ord a => (a -> [a]) -> ((Bool, Int) -> Bool) -> Set a -> Set a
conway genNearby evolve input = Set.filter (evolve . aliveAndCount) $ candidates
  where
    candidates = Set.fromList (concatMap genNearby input) `Set.union` input
    aliveAndCount p = (p `Set.member` input, count (`Set.member` input) (genNearby p)) 

