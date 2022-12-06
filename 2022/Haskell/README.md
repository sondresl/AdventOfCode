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

Note: These is indeed an [`interval` library](https://hackage.haskell.org/package/data-interval-2.1.1) 
that could be used here to avoid the overhead of creating full sets.

## Day 5

[Code](src/Day05.hs) | [Text](https://adventofcode.com/2022/day/5)

The tricky part about this puzzle is all about parsing the input, which has the
following form:

```txt
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
```

I split on `\n\n` and parse each section individually.

For the stacks it is possible to transpose the input, leaving all the relevant
parts for each stack on the same line, which is much easier to work with.

After reversing the entire line, splitting it into lines, and transposing it
looks like this:

```
[" ]  ","3P  "," [  ","    "," ]]]","2MCD"," [[[","    "," ]] ","1ZN "," [[ "]
```

Some of the lines are garbage, but the ones we care about have the data we want
in the right order, so we grab that and create a map from the identifying integer to
the characters in each stack.

This code shows the entire process, with `f` turning string into a `Map`, and
`foldMap` putting everything in the same map.

```haskell
    f (x:xs) = IM.singleton (read [x]) (reverse $ filter (/= ' ') xs)

    out = foldMap f
        . filter (not . null) 
        . map (dropWhile (`notElem` "123456789")) 
        . transpose 
        . reverse 
```

For the commands, we simply split on lines, split each line into words, and the
pattern match on the resulting list of strings to get the numbers we care
about.

```haskell
cmds = map (parse . words)
parse ["move", read -> n, "from", read -> from, "to", read -> to] = (n, from, to)
```

I use `ViewPatterns` to read the numbers into `Int` while pattern matchin on
the list, allowing me to just return the tuple of ints.

Let us create some useful type synonyms.

```haskell
type Stacks = IntMap String
type Cmd = (Int, Int, Int)
```

And for actually solving the problem, we simply remove the first `n` elements
from the `from`-stack, and then put them in the front/on top of the `to`-stack.

```haskell
rearrange :: (String -> String) -> Stacks -> Cmd -> Stacks
rearrange f stack (n, from, to) = IM.adjust (new <>) to $ IM.adjust (drop n) from stack
  where new = f . take n $ stack IM.! from
```

This function is written to do that process for one `Cmd`, and then we `foldl`
this function over the list of commands, with the initial stack as our starting
value.

It also takes a function as its first argument that manipulates the part of the
stack being put moved, reversing it for part 1, and not doing anything (with
`id`) for part 2.

```haskell
foldl (rearrange reverse) stack cmds
```

Putting it all together, we just add a final `score`-function to get the actual
answer.

```haskell
main :: IO ()
main = do
  (stack, cmds) <- parseInput <$> readFile "../data/day05.in"
  let score = map head . IM.elems
  putStrLn . score $ foldl (rearrange reverse) stack cmds
  putStrLn . score $ foldl (rearrange id     ) stack cmds
```

## Day 6

[Code](src/Day06.hs) | [Text](https://adventofcode.com/2022/day/6)

Very simple puzzle today. The input is just a single line of characters, so 
I use `init` to strip the newline.

```haskell
input <- init <$> readFile "../data/day06.in"
```

The first task is to find the first consecutive stretch of characters in which
all the characters are unique for a given number of characters. Let us grab a
`slidingWindow` utility function, as well as `ordNub` which lets us remove
duplicates in a list, and `findIndex` which returns the first index on which
the supplied function returns true.

```haskell
import Lib (slidingWindow, ordNub)
import Data.List.Extra (findIndex)
```

To find the right index, we iterate over each window of `n` character, remove
any duplicates, and then check if the length of the list is still `n`. Once
this index has been found, we add `n` onto the index since we want the index of
the first character *after* such a sequence.

```haskell
  let solve n = fmap (+n) . findIndex ((== n) . length . ordNub) . slidingWindow n
```

For part one, call the function with `n = 4`, and for part two with `n = 14`.

```haskell
print $ solve 4 input
print $ solve 14 input
```
