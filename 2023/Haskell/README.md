# Advent of Code 2023 | Haskell

## Day 1

[Code](src/Day01.hs) | [Text](https://adventofcode.com/2023/day/1)

I use the `tails` function to look at every possible tail of a given string,
and then use `isPrefixOf` to look for a number at the beginning of each tail.
This avoids issues with numbers being *burned* once used for a number, and
yields every possible number in the string. Pick the first and last number
found in each string.

```haskell
solve :: [String] -> String -> Int
solve comps input = head opts * 10 + last opts
  where opts = mapMaybe findNumber $ tails input
        findNumber str = (`lookupJust` convert) <$> find (`isPrefixOf` str) comps
```

## Day 2

[Code](src/Day02.hs) | [Text](https://adventofcode.com/2023/day/2)

Use `V3` from the `linear` library to get the instances we want. The heavy
lifting is done once parsing is done, with `V3` representing `RGB`-values.

It lets us combine each cube pull from the bag into the total RGB for the
reveal using `liftA2 (+)`, as each dimension of the vector is added.

```haskell
  cubes = foldr (liftA2 (+)) 0 <$> sepBy cube (string ",")
```

Similarly for part 2, the same pattern is used with `max` instead to get each
maximal dimension.

It also supports `product`, resulting in the product of all the dimensions of a
single `V3`.

```haskell
  print $ sum $ map (product . foldr (liftA2 max) 0 . snd) input
```
