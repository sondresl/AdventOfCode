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
run :: (R2 t, Applicative t, Num (t Int)) => Lens' (t Int) (V2 Int) -> [t Int -> t Int] -> Int
run f = productOf (f . each) . foldl' (&) (pure 0)
```

The call to `pure` will create either a `V2 0 0` or a `V3 0 0 0` depending on
the type of `t` in the input list.

The function itself folds a list of functions over some starting value, and
then calculates the product of the first two fields. `(&)` is function
application flipped (`flip ($)`) to make it work with `foldl`.

To create the list of functions, map over the input list and alter the correct
values.  It is possible to use the `Num` instance on vectors and create
functions that add a vector to the accumulating value.

It is also possible to use a single mapping, since the two values for part 1
are present in part 2 as well, as the `aim` in the second part corresponds to
the `depth` from the first part.

```haskell
encode :: (String, String) -> V3 Int -> V3 Int
encode (str, read -> v) = 
  case str of 
    "up" -> (+ V3 0 0 (-v))
    "down" -> (+ V3 0 0 v)
    "forward" -> \(V3 x y z) -> V3 (x + v) (y + v * z) z
```

In the first part we want the `x` and `z` fields, and in part 2 we want the `x` and `y` fields, so
we pass in a `Lens'` to get those fields and take their product.

Since we are doing a single mapping, we can make it part of the parsing.

Since 

```haskell
input <- map (encode . tuple . words) . lines <$> readFile "../data/day02.in"
```
