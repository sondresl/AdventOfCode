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
