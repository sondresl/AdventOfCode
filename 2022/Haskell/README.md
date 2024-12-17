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

## Day 7

[Code](src/Day07.hs) | [Text](https://adventofcode.com/2022/day/7)

Let's first create some type definitions: the `System` is a `Map` where each
`Path` points to either just a value (for leaves) or a different path (for
folders).

```haskell
type Path = [String]
type System = Map Path [Either String Int]
```

Most of the task today is about parsing the input; this function uses `State`,
and works by being applied to one line at a time and gradually bulding up the
`Map`. I keep track of the current path, and change it when we `cd` into a new
folder.  Every time a file is listed, it is added to the current folder in the
map.

```haskell
parseInput :: String -> State (System, Path) ()
parseInput str = case splitOn " " str of
  ["$", "cd"] -> modify (second $ const ["/"])
  ["$", "cd", ".."] -> modify $ second tail
  ["$", "cd", dir] -> do
    path <- gets snd
    modify $ second (dir:)
    modify $ first (Map.insertWith (<>) path [Left dir])
  ["$", "ls"] -> pure ()   -- Noise
  ["dir", name] -> pure () -- Noise
  [read -> size, name] -> do
    path <- gets snd
    modify $ first (Map.insertWith (<>) path [Right size])
  e -> error (show e)
```

Once the `Map` is finished, it is transformed into a proper `Tree`, with each
node having its own size, as well as every sub-folder as children. Files are
not included in this transformation, as it turns out they are not relevant to
the problem once their sizes have been counted.

```haskell
directories :: System -> Tree Int
directories m = go ["/"]
  where
    go path = Node (fileSum + sum (map rootLabel folders)) folders
      where (folders, fileSum) = bimap (map (\n -> go (n:path))) sum . partitionEithers $ m Map.! path
```

For part 1, the tree is folded into a list, filtered for values `<= 100 000`,
and summed. Part 2 is similar, but the filtering uses a diff, and the answer is
the minimum of the remaining folders, rather than the sum.

```haskell
  let dirs = toList . directories . fst $ execState (traverse parseInput input) (Map.singleton ["/"] [], [])
  print $ sum $ filter (<= 100000) dirs
  let diff = 30000000 - (70000000 - maximum dirs)
  print $ minimum $ filter (>= diff) dirs
```

## Day 8

[Code](src/Day08.hs) | [Text](https://adventofcode.com/2022/day/8)

*Awaiting some potential cleanup.*

## Day 9

[Code](src/Day09.hs) | [Text](https://adventofcode.com/2022/day/9)

For this task I used `V2` from the `Linear`-package to represent points, a
trick I picked up by reading solutions by [Justin Le](https://github.com/mstksg):

```haskell
V2 1 1 + V2 2 2 = V2 3 3
abs (V2 (-1) 1) = V2 1 1
signum (V2 (-10) 10) = V2 (-1) 1
```

I import some helpers from a file to have easy access to the unit vectors
representing different directions.

```haskell
import Advent.Coord (origin, right, left, up, down, Coord)
```

When parsing, I replicate each direction `n` times instead of having a single
vector representing the entire direction, to make it easier to keep track of
the number of steps taken.

```haskell
parseInput :: String -> [Coord]
parseInput = concatMap (f . words) . lines
  where
    f ["R", read -> n] = replicate n right
    f ["L", read -> n] = replicate n left
    f ["U", read -> n] = replicate n up
    f ["D", read -> n] = replicate n down
    f e = error (show e)
```

There are some basic patterns that appear in functional programming, that I
make use of The overal function to create the tail for a given list of
movements by a head has the type `[Coord] -> [Coord]`. This means we can use
`iterate :: (a -> a) -> a -> [a]` to have that function repeatedly applied to
its own result, generating a list of tails.

Another common function is `foldl :: (b -> a -> b) -> b -> [a] -> b`, folding
many values into a single value. I use the closely related `scanl` to keep
track of all intermediate values while folding. With this I can fold the
movements over a starting position to get the full path moved by a snake.

```haskell
runTail :: [Coord] -> [Coord]
runTail = scanl go origin
  where
    oneAway x y = abs (x - y) < 2
    go p@(V2 px py) x@(V2 xx xy)
      | oneAway px xx, oneAway py xy = p
      | otherwise = p + signum (x - p)
```

The original head path is found by scanning `(+)` over the input. To get the
desired generation of tail, I grab the appropriate index in the list generated
by `iterate`.

```haskell
  let headPos = scanl (+) origin input
      tailPos n = length . ordNub . (!! n) . iterate runTail $ headPos
  print $ tailPos 1
  print $ tailPos 9
```

## Day 10

[Code](src/Day10.hs) | [Text](https://adventofcode.com/2022/day/10)

Part 2 today is classic AOC, reading off something displayed in the terminal.

I ended up parsing the input into a list of functions, simply by trying to
parse every word in the input file. The words cannot be parsed into numbers and
become `Nothing` (using `readMaybe`), while the numbers are parsed into ints.
The `Nothing`s are converted into `id`, which does nothing with its input but
return it, and the numbers are partially applied to `(+)`. This means `noop` is
converted into a single `id`, and `addx n` converted into an `id` and a plus,
which matches with the number of cycles it takes to execute each command.

```haskell
parseInput :: String -> [Int -> Int]
parseInput = (id:) . concatMap (map (maybe id (+) . readMaybe) . words) . lines
```

Part 1 is simply scanning (again!) `flip ($)` with initial value `1` over the
functin list (function application must be flipped to apply the function to the
value, and not the value to the function). Then just grab the requested indexes
in the resulting list and sum them for part 1.

```haskell
let res = scanl (flip ($)) 1 input
print . sum . map ((*) <*> (res !!)) $ [20,60,100,140,180,220]
```

For part 2, `lightCRT` takes a tuple of `(cycle, value)` to determine whether
the *sprite*-position is such that the given pixel should be lit or not.  Since
each tuple is independent, this function is mapped over the list of
cycle-indexed values, and the results are put in a `Set` containing all
the lit coordinates.

```haskell
lightCRT :: (Int, Int) -> Maybe (V2 Int)
lightCRT (cycle, val) = guard inSprite *> Just (V2 col row)
  where
    (row, col) = (cycle - 1) `divMod` 40
    inSprite = col `elem` [pred val, val, succ val]
```

The set of points is sent to displaying function, and the result
must be parsed by eyesight.

```haskell
  input <- parseInput <$> readFile "../data/day10.in"
  putStrLn . display . mapMaybe lightCRT $ zip [0..] res

```

As for the actual drawing I have a function `display :: f (V2 Int) -> String`
that will return a print-friendly string, automatically finding the bounds of
the points to be drawn, filling in every point in a way that makes it very
readable.


```haskell
display :: Foldable t => t (V2 Int) -> String
display points = unlines $ do
  let (minx, miny, maxx, maxy) = findBounds points
  flip map [miny .. maxy] $ \y -> do
    flip map [minx .. maxx] $ \x -> do
      if V2 x y `elem` points
         then '▓'
         else ' '
```

```
▓▓▓▓ ▓  ▓ ▓▓▓  ▓  ▓ ▓▓▓▓ ▓▓▓  ▓  ▓ ▓▓▓▓
▓    ▓ ▓  ▓  ▓ ▓  ▓ ▓    ▓  ▓ ▓  ▓    ▓
▓▓▓  ▓▓   ▓  ▓ ▓▓▓▓ ▓▓▓  ▓  ▓ ▓  ▓   ▓
▓    ▓ ▓  ▓▓▓  ▓  ▓ ▓    ▓▓▓  ▓  ▓  ▓
▓    ▓ ▓  ▓ ▓  ▓  ▓ ▓    ▓    ▓  ▓ ▓
▓▓▓▓ ▓  ▓ ▓  ▓ ▓  ▓ ▓▓▓▓ ▓     ▓▓  ▓▓▓▓
```


## Day 11

[Code](src/Day11.hs) | [Text](https://adventofcode.com/2022/day/11)

Two parts require energy today; a bit for parsing, and then a lot for modular
arithmetic. It is pretty straight forward once understood, but that took a
while.

The parsing should be understandable if one is familiar with `Parsec`. The
interesting bit is the final grouping: By having each section return three
things;
1. a tuple `(Int, Monkey)` where `type Monkey = (Int -> Int, Int -> Int)`. The
   two functions are for generating the new level of worry, and to find the
   next monkey ID given the level of worry.
2. a list of tuples `(Int, Int)` being the starting monkey and starting worry
   level for a particular item.
3. an `Int` being the number this monkey test for divisibility with.

```haskell
type Monkey =
  ( Int -> Int -- Increase worry level
  , Int -> Int -- Test on worry level, return monkey throwing to
  )

parseInput :: String -> [((Int, Monkey), [(Int, Int)], Int)]
parseInput = either (error . show) id . traverse (parse p "") . splitOn "\n\n"
  where
    p = do
      num   <- read <$> (string "Monkey " *> many1 digit <* string ":" <* newline)
      items <- string "  Starting items: " *> sepBy (many1 digit) (string ", ") <* newline
      op    <- string "  Operation: new = old " *> oneOf "*+"
      value <- try (char ' ' *> many1 digit <* newline) <|> (char ' ' *> string "old" <* newline)
      test  <- read <$> (string "  Test: divisible by " *> many1 digit <* newline)
      true  <- read <$> (string "    If true: throw to monkey " *> many1 digit <* newline)
      false <- read <$> (string "    If false: throw to monkey " *> many1 digit)
      let opF = case (value, op) of
                  ("old", '*') -> (^2)
                  (    n, '*') -> (read value *)
                  (    n, '+') -> (read value +)
                  _ -> error "opF"
          nextF y = if y `mod` test == 0 then true else false
      pure ((num, (opF, nextF)), map ((num,) . read) items, test)
```

This list of 3-tuples are `unzip3`ed into three separate lists, one with
monkey-functions, one with all the starting item-values, and one with all the
divisibility tests.

The `runGift`-function calculates one inspection by a monkey, and returns a
tuple of the next monkey inspecting it, and the new worry level. By looking up
the current monkey in the `Map`, we get access to its `op` and `next`
functions. A *calming*-function is also passed in, which detemines how to lower
the worry-level each time to keep it under control.

By partially applying this function to a calming function and the list of
monkey-functions, it is possible to iterate it with a `(monkeyId,
worry)`-tuple, producing a list of these tuples. To separate this list into
rounds, find every time a monkey passes it to a lower-ID monkey, since it then
won't be inspected until the following round.

```haskell
runGift
  :: (Int -> Int)   -- Calming function, keeping the worry level in check
  -> Map Int Monkey -- Monkey id to functions
  -> (Int, Int)     -- (current monkey ID, worry level)
  -> (Int, Int)     -- New of ^^
runGift calmF mp (mnk, worry) = ((,) =<< next) $ calmF (op worry)
  where (op, next) = mp Map.! mnk

countRounds
  :: Int          -- Total number of rounds we want to play
  -> [(Int, Int)] -- Every played step of monkey and worry level
  -> Map Int Int  -- Number of inspections by monkey
countRounds rs = freqs . f 0 . (zip <*> tail)
  where
    f round [] = error "countRounds should always terminate"
    f round (((a,b), (x,y)):xs)
      | round == rs = []
      | x < a = a : f (succ round) xs
      | otherwise = a : f round xs
```

`countRounds` also figures out the frequency of each monkey inspection, so by
combining these frequencies for every starting value the combined inspection
count of every monkey is found. The score is then to multiply the two most
prolific inspection numbers.

```haskell
  let run n f = Map.unionsWith (+) $ map (countRounds n . iterate (runGift f input)) gifts
      score = product . take 2 . sortOn Down . Map.elems
```

For part 1 we are told to divide by `3` to calm our worry-levels. For part 2
this is no longer relevant, but we have to find different way to avoid
enourmous primes wile still keeping the integrity of our divisibility test.

The answer is to `modulo` by the product of all the divisibility tests. This
limits the upper bound of the number, while simultaneously preserving the
validity of division.

```haskell
  print . score $ run    20 (`div`    3)
  print . score $ run 10000 (`mod` mods)
```
