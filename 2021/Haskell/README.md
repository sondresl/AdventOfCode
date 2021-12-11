# Advent of Code 2021 | Haskell

## Table of Contents

- [Day 1](#day-1)
- [Day 2](#day-2)
- [Day 3](#day-3)
- [Day 4](#day-4)
- [Day 5](#day-5)
- [Day 6](#day-6)
- [Day 7](#day-7)
- [Day 8](#day-8)
- [Day 9](#day-9)
- [Day 10](#day-10)
- [Day 11](#day-11)

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

#### Benchmarks

```
Time (mean ± σ):      31.8 ms ±   3.1 ms    [User: 19.2 ms, System: 18.0 ms]
Range (min … max):    18.4 ms …  38.3 ms    83 runs
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

#### Benchmarks

```
Time (mean ± σ):      28.6 ms ±   4.6 ms    [User: 13.7 ms, System: 18.6 ms]
Range (min … max):    16.3 ms …  36.4 ms    81 runs
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

#### Benchmarks

```
Time (mean ± σ):      31.6 ms ±   2.7 ms    [User: 27.9 ms, System: 23.8 ms]
Range (min … max):    26.6 ms …  43.1 ms    80 runs
```
 
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

#### Benchmarks

```
Time (mean ± σ):      59.4 ms ±   6.1 ms    [User: 122.0 ms, System: 89.2 ms]
Range (min … max):    50.7 ms …  69.7 ms    41 runs
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

#### Benchmarks

```
Time (mean ± σ):     427.4 ms ±  13.5 ms    [User: 1365.7 ms, System: 578.5 ms]
Range (min … max):   409.1 ms … 444.3 ms    10 runs
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

#### Benchmarks

```
Time (mean ± σ):      41.3 ms ±   4.2 ms    [User: 54.0 ms, System: 34.5 ms]
Range (min … max):    29.1 ms …  46.5 ms    59 runs
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

#### Benchmarks

```
Time (mean ± σ):      26.6 ms ±   5.7 ms    [User: 10.1 ms, System: 15.6 ms]
Range (min … max):    15.2 ms …  35.7 ms    95 runs
```
 
## Day 8

[Code](src/Day08.hs) | [Text](https://adventofcode.com/2021/day/8)

I am very happy with my overall solution today, mainly because I found use for
`loeb`.

```haskell
loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x
```

It takes a `Functor`, in my case a list, where each element is a function from
a functor of `a` to `a`, and the end result is a functor of `a`. Since my
functor is `List`, it is a list of functions that takes a list as an argument
and produces a `String`. What is so special about this function is that the
list passed to each function in this list *is the list itself*. So it
calculates a result in terms of itself, and so simulates a spreadsheet in a
way. [Read more about it
here.](https://github.com/quchen/articles/blob/master/loeb-moeb.md)

Parsing today takes each lines and splits it on the `|`, returning the two
lists on either side in a tuple.

```haskell
parseInput :: String -> [([String], [String])]
parseInput = map (f . words) . lines
  where
    f input = (map sort $ take 10 input, map sort $ drop 11 input)
```

Then, for every line, the mapping is calculated using loeb to refer to the 
parts of the result that is easy to find. Those cases are in this case the
signals of unique length; `1`, `4`, `7`, and `8`.

In `loeb`, `1` is found on the line `const . head $ lengths 2`. There should be
only one string of length two, and then we must use `const` since the answer
list itself will be passed to each element, but we just ignore it.

Here is the function for two: `head . (lengths 5 \\) . ([(!! 5), (!! 3)] <*>) .
pure`.  The uses of `!!` refers to lookups in the answer list, for the value
that end up in positions five and three. The key is to avoid circular
dependencies, which is done by finding signals that can be decrypted by looking
at the four number of unique length. Since `5` and `3` can be found this way,
two can be found by removing their result from the list of strings of length
five, as there should only be three such strings.

The result list has every string on the index corresponding to its number,
which makes it easy to create a map from that string to the number, which can
then be used to map outputs from a string to its correct number.

Part one then counts the occurrences of the unique-length strings, and part 2
transforms each output list into a number, and sums all the numbers.

```haskell
main = do
  input <- parseInput <$> readFile "../data/day08.in"
  let solve f = sum . map (\(signals, outputs) -> f $ map (determine signals Map.!) outputs)
  print $ solve (count (`elem` [1,4,7,8])) input
  print $ solve (unDigits 10) input
```

#### Benchmarks

```
Time (mean ± σ):      30.2 ms ±   3.0 ms    [User: 19.7 ms, System: 19.1 ms]
Range (min … max):    25.2 ms …  40.9 ms    86 runs
```

## Day 9

[Code](src/Day09.hs) | [Text](https://adventofcode.com/2021/day/9)

The first grid problem of the year.

I use my trusty `parseAsciiMap` function *borrowed* from [Justin
Le](https://github.com/mstksg/advent-of-code-2021) last year to parse the input
into a map from positions to ints.

```haskell
input <- parseAsciiMap (Just . read . pure) <$> readFile "../data/day09.in"
```

The function passed to is says (1) place the element in a list (this makes the
`Char` into a `String`), (2) turn that string into an int, and (3) place it
inside the `Just` data constructor. If there are any elements in the input I do
not care about, I can have the function return a `Nothing` for these elements,
and they will be ignored in the resulting map.

The low points are then found by comparing all points with their neighbours and
only keeping those with strictly greater values in neighbouring positions.

```haskell
lowPoints :: Map (V2 Int) Int -> [(V2 Int, Int)]
lowPoints input = Map.toList $ Map.filterWithKey lowPoint input
  where lowPoint k v = all (v <) . mapMaybe (`Map.lookup` input) $ neighbours4 k
```

`neighbours4` produces the four neighbours above, below, left, and right of a given
position. `mapMaybe` is used to keep only those neighbouring values that are actually
in the map, and this removes the need for bounds checking. The answer to part 1
is the value of each of these lowpoints incremented once and summed.

```haskell
sum . map ((+1) . snd) $ lps
```

For part 2, I do a BFS from each lowpoint and outwards. I have `bfs` implemented
from a previous year and spent way too much time remembering how to use it.

```haskell
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
```

By passing an initial list of elements and a function that generates new
candidates from the current candidate, it returns a list of all the candidates
in order of seeing them. 

The function passed in to generate new candidates generates the four neighbouring
positions, and then filter out those not in the map and those with a value of `9`.
I use a little trick here to filter out the values not in the map by giving them
the default value `9`, thus filtering them out as if they were land.

```haskell
filter (liftA2 (&&) (val <) (/= 9) . flip (Map.findWithDefault 0) input) . neighbours4
```

It is not really clear from the text that all basins are strictly increasing
and always bordered by only `9`s, but I found this to be the case.

The answer to part 2 is taking the three longest lengths of all the searches, corresponding
to the size of each basin, and taking the product of these three values.

```haskell
product . take 3 . sortBy (flip compare) $ map (basin input) lps
```

```
Time (mean ± σ):      76.5 ms ±   7.4 ms    [User: 106.6 ms, System: 58.0 ms]
Range (min … max):    61.6 ms …  91.9 ms    37 runs
```

## Day 10

[Code](src/Day10.hs) | [Text](https://adventofcode.com/2021/day/10)

```
Time (mean ± σ):      18.3 ms ±   1.4 ms    [User: 5.6 ms, System: 11.2 ms]
Range (min … max):    15.5 ms …  22.4 ms    136 runs
```

## Day 11

[Code](src/Day11.hs) | [Text](https://adventofcode.com/2021/day/11)

```
Time (mean ± σ):      45.8 ms ±   5.0 ms    [User: 37.0 ms, System: 16.3 ms]
Range (min … max):    39.5 ms …  56.7 ms    55 runs
``` 
