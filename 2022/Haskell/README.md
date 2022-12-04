# Advent of Code 2022 | Haskell

## Day 1

[Code](src/Day01.hs) | [Text](https://adventofcode.com/2022/day/1)

Parse the input by first splitting on double newlines, and parsing each block
on its own. This results in a list of `Int` sorted descendingly.

```haskell
input <- sortOn Down . map (sum . map read . lines) . splitOn "\n\n" <$> readFile "../data/day01.in"
```

For the first part, take the head of the list.

```haskell
print $ head input
```

For the second part, sum the first three entries in the list.

```haskell
print . sum . take 3 $ input
```

Pretty simple, as is expected of the first day.

## Day 2

[Code](src/Day02.hs) | [Text](https://adventofcode.com/2022/day/2)

Today we are playing rock, paper, scissors. There are many ways to encode,
perhaps the easiest being as numbers that represent the *value* of each move.
However, I decided to actually create a data class.

```haskell
data RPS = Rock | Paper | Scissors
  deriving (Enum, Show, Eq, Ord, Bounded)
```

For both parts, each line is an independent game, and we have to solve each
hand individually and sum the results. The actual score in both parts is the
outcome (6 for win, 3 for draw, 0 for loss) plus the move played (1 for `Rock`,
2 for `Paper`, 3 for `Scissors`).

One *trick* here is to have the datatype derive `Enum` and `Bounded`.  As
normal this lets us use `succ :: (Enum a, Bounded a) => a -> a` and `pred` to
get the succeding and preceding enum value from a given value.  However,
calling these functions on the the last or first enum will crash, to I have
created two helper function `safeSucc` / `safePred` which *rotates round* to
the start/end of the enum instead. For this problem it means we get the winning
play against a given value by calling `safeSucc` on that value, and vice versa
for losing and `safePred`.

For part 1, we are given the two moves and have to determine the score. The
outcome is determined by comparing our own move with 1) the winning move
required against the opponent move, or 2) just the opponent move for a draw, or
3) otherwise it must be a loss.


```haskell
part1 :: RPS -> RPS -> Int
part1 a b = outcome a b + score b
  where
    score r = let Just v = lookup r [(Rock, 1), (Paper, 2), (Scissors, 3)] in v
    outcome a b
      | b == safeSucc a = 6
      | a == b = 3
      | otherwise = 0
```

For part two we are told it was not our move, but a description of how we
*should* play, so we have to use the opponents move to determine how to play in
order to win, lose, or draw. `action` takes our strategy and returns a function
that will produce the correct move based on the opponents move.


```haskell
part2 :: RPS -> RPS -> Int
part2 a b = part1 a (action b a)
  where
    action = \case
      Rock -> safePred
      Paper -> id
      Scissors -> safeSucc
```

Parsing is done by splitting each line and just matching both side to the
datatype, even if it does not make much sense for part 2.

```haskell
parseInput a
  | a `elem` ["X", "A"] = Rock
  | a `elem` ["Y", "B"] = Paper
  | a `elem` ["Z", "C"] = Scissors
  | otherwise = error a
```

## Day 3

[Code](src/Day03.hs) | [Text](https://adventofcode.com/2022/day/3)

Day 3 requires us to analyze strings for common letters. We just parse the
input into a list of strings, one for each line.

```haskell
input <- lines <$> readFile "../data/day03.in"
```

Common letters are scored on a simple system of 1 for `'a'`, up to
52 for `'Z'`.

```haskell
priority = fromJust . (`lookup` zip (['a'..'z'] <> ['A'..'Z']) [1..])
```

The key function, `scoreCommon`, takes in a list of strings and takes *folds*
the `intersect` function over the list to produce a string with only letters
common to all strings. We have to `nub` in order to remove duplicate letters.
If we converted to a `Set`, we would avoid this step, but then we would need
additional scaffolding for translating between strings and sets. In practice,
we are dealing with fairly short strings, so performance is a non-issue.

```haskell
scoreCommon = sum . map priority . nub . foldl1 intersect
```

For part 1, we produce the list of strings by splitting each line in half and
comparing the two halves for common letters, and the scoring these letters.

```haskell
halve str = [a,b] where (a,b) = splitAt (length str `div` 2) str
print . sum . map (scoreCommon . halve) $ input
```

For part two we chunk three and three lines together and compare.

```haskell
  print . sum . map  scoreCommon . chunksOf 3 $ input
```

## Day 4

[Code](src/Day04.hs) | [Text](https://adventofcode.com/2022/day/4)

Those of us hoping for something more difficult on a Sunday were disappointed,
as today was very simple.

The input had each line be two tuples of numbers, on the form `d,d-d,d`. I
parsed this by simple splitting and pattern matching, and ended up creating
actual sets of numbers for the solving. In terms of efficiency this is a bad
idea (space-wise, particularly), as the problems are very easily solved by just
comparing the values, but I found this more enjoyable.

```haskell
input <- map parseInput . lines <$> readFile "../data/day04.in"
(...)
parseInput :: String -> (IntSet, IntSet)
parseInput str = (fromList [a..b], fromList [c..d])
  where
    [[a,b], [c,d]] = map (map read . splitOn "-") . splitOn "," $ str
```

Part one asks to determine if one set is a subset of the other. Since this can
go both ways, it requires two checks, and the results are `or`ed. The answer is
just counting how many lines this is true for.

```haskell
  print $ count ((||) <$> uncurry isSubsetOf <*> uncurry (flip isSubsetOf)) input
```

For part two it is a simple `disjoint`-check, and a line *counts* is the sets
are *not* disjoint.

```haskell
  print $ count (not . uncurry disjoint) input
```
