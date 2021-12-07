# Advent of Code 2021 | Haskell

## Table of Contents

- [Day 1](#day-1)
- [Day 2](#day-2)
- [Day 3](#day-3)
- [Day 4](#day-4)
- [Day 5](#day-5)
- [Day 6](#day-6)
- [Day 7](#day-7)

## Day 1 

[Code](src/Day01.hs) | [Text](https://adventofcode.com/2021/day/1)

`(<*>)` applied to functions becomes the `S`-combinator, which means it takes two
functions and one input as argument, and applies the first function to the input
and the result of calling the second function on the input.

`(<*>) f g x = f x (g x)`

In this case, I use it to zip a list with itself, after first applying a function
 to the list.

If the function applied to the list is `tail`, we get a list of tuples where
 each tuple contains succeeding elements.

`(zip <*> tail) [1,2,3,4] = [(1,2), (2,3), (3,4)]`

So for part 1, checking how many tuples with the second element greater than the first 
is the answer.

For part 2, we can see taht comparing two sliding windows for their sum is
actually just comparing the first and last elements since the rest are shared,
so by creating tuples of elements separated by two elements, we can reuse the
logic from part 1 by using `drop 3` instead of `tail`.

Since the only difference between the solutions is the function passed, we can create a higher-order function to encode this:

```haskell
  let solve f = count (uncurry (<)) . (zip <*> f)

  print $ solve tail input
  print $ solve (drop 3) input
```

## Day 2

[Code](src/Day02.hs) | [Text](https://adventofcode.com/2021/day/2)

This has gone through a few iterations, but it is (probably finished) now!

Each instruction in the input describes a way to alter the current position.
This change can be described as function `Position -> Position`; given one
position, it will produce a new position. If you have many functions on the
form `a -> a`, they can be composed together end to end and produce one single
`a -> a`-function.

The datatype that encodes this is `Endo`.

```haskell
newtype Endo a = { appEndo :: a -> a }

instance Semigroup Endo where
  -- Combining functions is composition
  (<>) = (.)

instance Monoid Endo where
  mempty = id
```

`encode` takes our input an creates functions on the form `V3 Int -> V3 Int`,
which can be placed into an `Endo`.

```haskell
encode :: (String, String) -> V3 Int -> V3 Int
encode (str, read -> v) =
  case str of 
    "up" -> (+ V3 0 0 (-v))
    "down" -> (+ V3 0 0 v)
    "forward" -> \(V3 x y z) -> V3 (x + v) (y + v * z) z
```

The actual mapping happens during *parsing*, with `intoEndo (encode . tuple .
words)` defined as `getDual . foldMap (Dual . Endo . f)`. The dual is necessary
to flip the order of function composition, otherwise the functions would run
the computation back-to-front.

To run the combined function, provide a starting value for the function. `productOf`
is a `Lens` that takes the product of all the elements it *sees* in the target. This is
used to differentiate `run` for part 1 and part 2.

```haskell
run :: Lens' (V3 Int) (V2 Int) -> Endo (V3 Int) -> Int
run f = productOf (f . each) . (`appEndo` V3 0 0 0)
```


## Day 3

[Code](src/Day03.hs) | [Text](https://adventofcode.com/2021/day/3)

The code today makes heavy use of `transpose` to get easy access to all the
elements on the same index. For the first part, the result of each part is
independent and can be calculated on its own before being combined. `gammaEpsilon`
uses the monoid instance on strings and tuples to do this.

```haskell
gammaEpsilon :: [String] -> (String, String)
gammaEpsilon = foldMap go . transpose
  where
    go str
      | count (== '1') str >= count (== '0') str = ("1", "0")
      | otherwise = ("0", "1")

```

If both elements of a tuple is a monoid, then the whole tuple is a monoid.

`(x0, y0) <> (x1, y1) = (x0 <> x1, y0 <> y1)`

So on each line, a `("1", "0")` or `("0", "1")` is created, and all those tuples
combine to have *gamma* as the first element and *epsilon* as the second.

The actual solution the requires converting both binary strings to integers and
multiplying them.

```haskell
part1 = uncurry (*) . both binToInt . gammaEpsilon
```

Part 2 makes each successive `gammaEpsilon`-calculation depend on the previous
one.  However, at each level we know that the current bit is the corresponding
`gammaEpsilon`-bit for the line, and then we can use that bit to filter the
list for the next iteration, and then take the tail of every remaining
candidate to avoid having to deal with indexing.

```haskell
keep :: ((String, String) -> String) -> [String] -> String
keep _ [x] = x
keep f strs =
  let val = head . f $ gammaEpsilon strs
    in val : keep f (map tail $ filter ((== val) . head) strs)
```

Since we need to do two passes, one for picking the most prevalent bit, and one
for the least, I use `(&&&)` to call two functions on the same input and
produce a tuple of results. This tuple can be passed to `run` to have each side
converted into an int and multiplied.

## Day 4

[Code](src/Day04.hs) | [Text](https://adventofcode.com/2021/day/4)

Today we are told to find the first and last bingo board to get *bingo*
based on a list of numbers.

My *trick* is to, for each board, to add the transposed board to the list of
rows, thus getting the list of columns. Since each board now has both all rows
and columns, I can filter each incoming number out of all lists, and once a
board has one empty list it means that either a row or column has been
completed. I can then grab the first five sublists, being the original rows,
and sum them to get the `score`.

Playing one board, it scans over the input list, returning a list of boards
where one digit at a time is *checked out*. When one sublist is empty, return
tuple with the score of the final *board*, and the turn on which it happened.
This is the same for both parts.

```haskell
play :: [Int] -> Board -> (Int, Int) -- Score, turns
play nums board = (score . last &&& subtract 1 . length)
                . tail 
                . takeUntil (any null) 
                $ scanl (flip $ map . filter . (/=)) board nums
  where 
    score = sum . map sum . take 5
```

To play all boards, map `play` over them, having added the transposed rows to
each board. This returns a list of tuples of scores and turns. When this list
is sorted, the first and last elements correspond to part 1 and part 2
respectively. The answer for both parts require each score and turn to be 
multiplied together.

```haskell
playAll :: [Int] -> [Board] -> Result
playAll xs = both (uncurry (*)) 
           . (head &&& last) 
           . map (second (xs !!)) 
           . sortOn snd 
           . map ((play xs) . ((<>) <*> transpose))
```

## Day 5

[Code](src/Day05.hs) | [Text](https://adventofcode.com/2021/day/5)

The solution today takes all the pairs of coordinates, generates every total
path between them, collects them in a frequency map, and counts the number of
elements appearing at least twice.

First, each line is parsed into a tuple of tuples, with the inner tuples represented 
by the `V2` type, a vector type. This type has a very useful `Num` instances.
I use `parsec` to parse the input string directly into a list of this type.

```haskell
parseInput :: String -> [(V2 Int, V2 Int)]
parseInput = either (error . show) id . traverse (parse line "") . lines
  where
    num = read <$> many1 digit
    tuple = V2 <$> (num <* char ',') <*> num
    line = (,) <$> tuple <* string " -> " <*> tuple
```

I created a helper function `lineSegment` which takes two vectors and generate
every point in between, inclusive of both ends. It does so by finding the unit
vector pointing from one to the other, and the iteratively adding it to the
original position until the target position is reached, returning all the
intermediate results in a list. The somewhat advanced type signature is to make
it work over vectors of different dimensions.

```haskell
lineSegment :: (Functor t, Ord a, Ord (t a), Num (t a), Integral a) => t a -> t a -> [t a]
lineSegment x0 x1 = takeUntil (== x1) $ iterate (+ dir) x0
  where dir = fmap (safeDiv <*> abs) (x1 - x0)
        safeDiv x y = if y == 0 then 0 else x `div` y
```

Since parsing producus tuples of start and end position, I `concatMap` 
the `lineSegment`-function over this list to get all the points for all the pairs.
This list is folded into a frequency map with the `freqs` utility function.

```haskell
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = Map.fromListWith (+) . map (,1) . toList
```

Since maps are `Foldable`, `count` can be used to find all the frequencies that
occur more than once. The, the only thing needed is to filter out all the
diagonal line segments for part 1, by finding pairs of coordinates where either
the `x`-coordinates or the `y`-coordinates are the same.

```haskell
main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day05.in"
  let run = count (> 1) . freqs . concatMap (uncurry lineSegment)
  print $ run (filter (uncurry $ foldr1 (||) .: liftA2 (==)) input)
  print $ run input
```

## Day 6

[Code](src/Day06.hs) | [Text](https://adventofcode.com/2021/day/6)

Instead of actually making the lists, I use a frequency map to keep track of
how many occurrences there are of fish in each state. Then, each generation,
decrement the *key*, and use the `-1` key to populate the new fish in state `6`
and `8`.

The function `step` simulates one generation.

```haskell
step :: Map Int Int -> Map Int Int
step input = Map.insert 8 negs 
           . Map.insertWith (+) 6 negs 
           $ Map.insert (-1) 0 input'
  where 
    input' = Map.foldMapWithKey (Map.singleton . subtract 1) input
    negs = Map.findWithDefault 0 (-1) input'
```

To solve the problem, `iterate` this step function 80 times for part 1 and 256
times for part 2, and sum the values to get the answers.

```haskell
let run n = sum . (!! n) . iterate step
print $ run 80 input
print $ run 256 input
```

## Day 7

[Code](src/Day07.hs) | [Text](https://adventofcode.com/2021/day/7)

We can use math today to solve the problems. 

The actual *solver* takes a number and a list of positions, and then sums the
distances between each position and this value. In part 2, the cost is further
as the triangle number of the distance.

`run` lets us pass this function that calculates the cost from the distance.
For part 1, pass in `id` to do nothing.

```haskell
run f val = sum . map (f . abs .  subtract val) $ input
```

In the first part, the cost of moving is linear, and so we want to move
outliers towards where most of the crabs are. Find the median crab, and move
all crabs to it.

In the second part, the cost of moving increases with distance, so it is better
to have many crabs move shorter distances. Find the mean and move all the crabs
there.  Since the mean can be a floating point, check both the floor and
ceiling of this value to find the minimum cost.

```haskell
digitSum x = x * (x + 1) `div` 2
median = input !! (length input `div` 2)
mean = map ((+) $ sum input `div` length input) [0, 1]

print $ run id median
print $ minimum $ map (run digitSum) mean
```

`digitSum` is used to calculate the value of the n'th triangle number.

