{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Text.ParserCombinators.Parsec (Parser, parse, noneOf, sepEndBy, many, digit, try, (<|>), char, many1)
import Control.Comonad.Store (ComonadStore(experiment))
import Control.Lens hiding (noneOf)
import Control.Monad ((<=<), guard, foldM)
import Data.Array (accumArray, elems)
import Data.Bool (bool)
import Data.Monoid (Dual(..), Endo(..))
import Data.Foldable ( Foldable(foldl', toList), for_ )
import Data.List.Extra (sort, transpose, tails, inits, splitOn, chunksOf, minimumBy, maximumBy)
import Data.Semigroup (Max (Max, getMax), Min (Min, getMin))
import Linear ( V2(..), _x, _y, R2 )
import Data.Function ( on )
import Data.MemoTrie (HasTrie, mup, memo3, (:->:), untrie, trie, enumerate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set ( Set )
import Data.Maybe (listToMaybe, fromJust, isJust)
import qualified Data.Sequence as Seq
import           Data.Sequence ( Seq, empty )

safeSucc, safePrev :: (Eq a, Bounded a, Enum a) => a -> a
safeSucc a = if a == maxBound then minBound else succ a
safePrev a = if a == minBound then maxBound else pred a

(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(.:) = (.) . (.)

isPrime :: Integral a => a -> Bool
isPrime n = null $ do
              x <- 2 : [ 3, 5 .. round (sqrt $ fromIntegral n) ]
              guard $ n `mod` x == 0
              pure x

count :: Foldable t => (a -> Bool) -> t a -> Int
count p = length . filter p . toList

everyOther :: [a] -> [a]
everyOther (x:_:xs) = x : everyOther xs
everyOther (x:_) = [x]
everyOther [] = []

middle :: (Foldable t, Ord a) => t a -> a
middle (toList -> xs) = last $ zipWith const xs (everyOther xs)

binToInt :: String -> Int
binToInt = foldl ((+) . (*2)) 0 . map (read . pure)

-- To get _all_ diagonals, do this once on the list and on the reversed list
diagonals :: [[a]] -> [[a]]
diagonals xs = lower <> upper
  where
    upper = transpose $ zipWith drop [0..] xs
    lower = reverse $ transpose $ map reverse $ zipWith take [0..] xs

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n = filter ((== n) . length) . go
  where 
    go [] = []
    go xs = take n xs : go (drop 1 xs)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f [] = error "takeUntil: Predicate not fulfilled!"
takeUntil f (x:xs)
  | not (f x) = x : takeUntil f xs
  | otherwise = [x]

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
  pure (t, is <> ts)

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
tuple _ = error "List does not contain exactly two elements"

tuple3 :: [a] -> (a, a, a)
tuple3 [a, b, c] = (a, b, c)
tuple3 _ = error "List does not contain exactly three elements"

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


dijkstra :: Ord a => Map a [(a, Int)] -> a -> Map a Int
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
  (Ord a, Ord b) =>
  [a] -> -- Initial candidates
  (a -> [a]) -> -- Generate new candidates from current
  (a -> b) -> -- Project into set of seen
  [a] -- All the visited 'areas'
bfs start fn project = go Set.empty (Seq.fromList start)
  where
    go seen next
      | Seq.null next = []
      | otherwise = let (c Seq.:<| cs) = next
      in let cands = filter (not . (`Set.member` seen) . project) $ fn c
             seen' = seen <> Set.fromList (map project cands)
       in c : go (Set.insert (project c) seen') (cs Seq.>< Seq.fromList cands)

dfs :: (Ord r, Ord a) => (a -> r) -> (a -> [a]) -> a -> [a]
dfs repr next start = go Set.empty [start]
  where
    go _ [] = []
    go seen (x:xs)
      | Set.member r seen = go seen xs
      | otherwise = x : go (Set.insert r seen) (next x <> xs)
     where
       r = repr x

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

maximumVal' :: Ord b => Map a b -> (a, b)
maximumVal' = fromJust . maximumValBy compare

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

minimumVal' :: Ord b => Map a b -> (a, b)
minimumVal' = fromJust . minimumValBy compare
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
findLoop normalize [] = error "No input sequence"
findLoop normalize (x:xs) = go (Map.singleton x (0, 0)) 1 xs
  where
    go !seen !i [] = error "Input sequence not a loop"
    go !seen !i (w:ws) = case Map.lookup w'Norm seen of
                       Nothing -> go (Map.insert w'Norm (mn, i) seen) (i + 1) ws
                       Just (seenMn, seenI) -> (seenI, i - seenI, mn - seenMn)
      where
        (mn, w'Norm) = normalize w

findLoopSimple :: Ord a => [a] -> (Int, Int) -- first loop, size of loop, shift
findLoopSimple = firstTwo . findLoop (0,)
  where firstTwo (x,y,_) = (x,y)

-- Given tuples of tuples of 'time' and some period, when do they match up again
-- and what is the new period
sync :: (Int, Int) -> (Int, Int) -> (Int, Int)
sync (time, period) (time', period') =
  let candidates = dropWhile (< time) $ iterate (+ period') time'
      first = head $ dropWhile ((/= 0) . (`mod` period) . subtract time) candidates
   in (first, lcm period period')

-- Iterate a function n times
iterateN :: Int -> (a -> a) -> a -> a
iterateN n f = (!! n) . iterate f

-- Iterate a function until it returns Nothing.
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : case f x of
                         Nothing -> []
                         Just v -> iterateMaybe f v

toMapIndexed :: [a] -> Map Int a
toMapIndexed = Map.fromList . zip [0..]

intoEndo :: Foldable t => (a -> b -> b) -> t a -> Endo b
intoEndo f = getDual . foldMap (Dual . Endo . f)

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
freqs :: (Foldable f, Ord a, Integral b) => f a -> Map a b
freqs = Map.fromListWith (+) . map (,1) . toList

-- | Useful functions for displaying some collection of points in 2D
-- Works for tuples and V2s
findBounds ::
  (Foldable t, R2 f) =>
  -- | The points in 2D space
  t (f Int) ->
  -- | V2 (V2 minX minY) (V2 maxX maxY)
  (Int, Int, Int, Int)
findBounds cs = (getMin minX, getMin minY, getMax maxX, getMax maxY)
 where
  (minX, minY, maxX, maxY) = foldMap f cs
  f point = (Min $ view _x point,
             Min $ view _y point,
             Max $ view _x point,
             Max $ view _y point)

type Point = V2 Int

pointToTuple :: Point -> (Int, Int)
pointToTuple (V2 x y) = (x, y)

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map Point a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton
  where
    asciiGrid :: IndexedFold Point String Char
    asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

display :: Foldable t => t (V2 Int) -> String
display points = unlines $ do
  let (minx, miny, maxx, maxy) = findBounds points
  flip map [miny .. maxy] $ \y -> do
    flip map [minx .. maxx] $ \x -> do
      if V2 x y `elem` points
         then '▓'
         else ' '

-- | All eight surrounding neighbours
-- | Will generate neighbours for V2, V3, V4 +++
neighbours ::
    (Traversable t, Applicative t, Num a, Eq (t a)) =>
    t a ->
    [t a]
neighbours p = do
    n <- tail $ sequenceA (pure [0, 1, -1])
    pure $ (+) <$> p <*> n

lineSegment :: (Functor t, Ord a, Ord (t a), Num (t a), Integral a) => t a -> t a -> [t a]
lineSegment x0 x1 = takeUntil (== x1) $ iterate (+ dir) x0
  where dir = fmap (safeDiv <*> abs) (x1 - x0)
        safeDiv x y = if y == 0 then 0 else x `div` y


-- | Neighbours left, right, above and below
neighbours4 :: Point -> [Point]
neighbours4 p = (p +) <$> [V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1)]

mannDist :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
mannDist x y = sum . abs $ x - y

-- | Various simple parser

-- | Parse all numbers in a file; might replace both functions below
allNums :: String -> [Int]
allNums = either (error . show) id . parse p ""
  where 
    p = map read . filter (not . null) <$> (nonDigit >> sepEndBy oneNum nonDigit)
    nonDigit = many (noneOf "-0123456789")
    oneNum = try ((:) <$> char '-' <*> many1 digit) <|> many1 digit <|> (char '-' >> pure "")

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
conway genNearby evolve input = Set.filter (evolve . aliveAndCount) candidates
  where
    candidates = Set.fromList (concatMap genNearby input) `Set.union` input
    aliveAndCount p = (p `Set.member` input, count (`Set.member` input) (genNearby p)) 

hexToBin :: Integral n => Char -> [n]
hexToBin = \case
  '0' -> [0,0,0,0]
  '1' -> [0,0,0,1]
  '2' -> [0,0,1,0]
  '3' -> [0,0,1,1]
  '4' -> [0,1,0,0]
  '5' -> [0,1,0,1]
  '6' -> [0,1,1,0]
  '7' -> [0,1,1,1]
  '8' -> [1,0,0,0]
  '9' -> [1,0,0,1]
  'A' -> [1,0,1,0]
  'B' -> [1,0,1,1]
  'C' -> [1,1,0,0]
  'D' -> [1,1,0,1]
  'E' -> [1,1,1,0]
  'F' -> [1,1,1,1]
  e -> error ("Bad hex value: " <> show e)

memo4 :: (HasTrie a, HasTrie b, HasTrie c, HasTrie d, HasTrie e) =>
     (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
memo4 = mup memo3

instance HasTrie (V2 Int) where
  newtype (V2 Int) :->: a = V2Trie (Int :->: Int :->: a)
  trie f = V2Trie (trie $ \y -> trie $ \x -> f (V2 y x))
  V2Trie t `untrie` V2 y x = t `untrie` y `untrie` x
  enumerate (V2Trie t) = [(V2 y x, a) | (y, xs) <- enumerate t, (x, a) <- enumerate xs]
