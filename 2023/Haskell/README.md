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
