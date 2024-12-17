# Advent of Code 2021 | Haskell

## Table of Contents

- [Day 1](#day-1)
- [Day 2](#day-2)
- [Day 3](#day-3)

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
