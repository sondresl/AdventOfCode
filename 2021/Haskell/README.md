# Advent of Code 2021 | Haskell

## Day 1

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

When working with `Linear` and `V2/V3` it is fun trying to generalize functions
to work over multiple dimensions ([see day 17 from 2020](../../2020/Haskell/src/Day17.hs)).

```haskell
run :: (R2 t, Applicative t, Num (t Int)) => [t Int -> t Int] -> Int
run = productOf (_xy . each) . foldl' (&) (pure 0)
```

- The call to `pure` will create either a `V2 0 0` or a `V3 0 0 0` depending on the type
 of `t` in the input list.
- The `_xy`-lens requires at least two fields (so no `V1`), since we want access to both 
 the `x` and `y` fields on the vector.

The function itself folds a list of functions over some starting value, and then
calculates the product of the first two fields. `(&)` is function application flipped
(`flip ($)`) to make it work with `foldl`.

To create the list of functions, map over the input list and alter the correct values.
It is possible to use the `Num` instance on vectors and create functions that add
a vector to the accumulating value.

```haskell
part1 :: [(String, Int)] -> Int
part1 = run . map f 
  where
    f ("up", v) = (+ V2 0 (-v))
    f ("down", v) = (+ V2 0 v)
    f ("forward", v) = (+ V2 v 0)

part2 :: [(String, Int)] -> Int
part2 = run . map f
  where
    f ("up", v) = (+ V3 0 0 (-v))
    f ("down", v) = (+ V3 0 0 v)
    f ("forward", v) = \(V3 x y z) -> V3 (x + v) (y + (v * z)) z
```

It is possible to parse more and create actual types, but I decided not to do that.

```haskell
parseInput :: String -> [(String, Int)]
parseInput = map (f . words) . lines
  where
    f [x,y] = (x, read y)
```
