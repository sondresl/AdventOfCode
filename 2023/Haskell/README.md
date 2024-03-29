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

## Day 3

[Code](src/Day03.hs) | [Text](https://adventofcode.com/2023/day/3)

Spent a lot of time thinking and messing around with different representations.
Ended up going over the grid and finding each number by only looking at the
leftmost digit (that is, digits with no digit to the left), finding the entire
number, and then finding the associated symbol by looking at all neighbours. By
associating each symbol with all surrounding number, part 1 is to just sum all
the numbers and part two is summing the products of all numbers associated with
a gear (that has two numbers).


```haskell
  -- Part 1
  print $ sum (sum <$> findNumbers input)

  --- Part 2
  print . sum . map (product . snd) . filter ((&&) <$> isGear . fst <*> ((==2) . length . snd)) . Map.assocs $ findNumbers input
```

## Day 4

[Code](src/Day04.hs) | [Text](https://adventofcode.com/2023/day/4)

The input is parsed by splitting each line on `|` and finding all the numbers
in each half, and then discarding the id.

```haskell
parseInput :: String -> [([Int], [Int])]
parseInput = map (first tail . tuple . map allNums . splitOn "|") . lines
```

Each *round* is now represented by a tuple of two list, and the amount of
winning numbers is found with the intersection of these lists.

```haskell
let winCount = length . uncurry intersect
print . sum . map ((2^) . subtract 1) . filter (>0) . map winCount $ input
```

Part 2 requires counting the total number of cards, and each card can increase
the number of cards that appear later, and recursively this can create a lot of
cards. There is a neat way to count from the back of the list with `foldr`, but
I have left my initial solution.

To avoid explicit recursion, I wrote a function that can be passed to `unfoldr`.
A new list is constructed where the value of each card is increased appropriately
by preceding cards, until its value is added to the new list, and the original
list is altered along the way. The final list is summed to find the answer.

```haskell
accumulate :: [(Int, Int)] -> Maybe (Int, [(Int, Int)])
accumulate [] = Nothing
accumulate ((c, wins):xs) = Just (c, map (first (+ c)) (take wins xs) <> drop wins xs)
```


## Day 5

[Code](src/Day05.hs) | [Text](https://adventofcode.com/2023/day/5)

The trick of the day is to use `IntervalMap`s to represent each transformation.
Each line of `dest src range` is represented as `(src <=..< src + range, dest - src)`, 
so if you look up a number in an interval, you get back the amount to add to 
your number to get the next number.

So start with an interval of numbers, and fold it through the list of transformation
to end up with a list of locations. We are only interested in the final locations and
not the initial seeds, which makes it easier.

For part 2, fold through the initial seed intervals, and find the minimum lower
bound of all the resulting intervals.

For part 1, treat each seed as a singleton interval and use the same function.

## Day 6

[Code](src/Day06.hs) | [Text](https://adventofcode.com/2023/day/6)


Today was simple and can easily be brute solved as the magnitude of part 2 is
not sufficient to require a more advanced solution, and can also be solved
mathematically if one is so inclined.

However, I used a binary search I had lying around in my library to first find
the lower bound of wins, and then did a new binary search between that lower
bound and the end to find the upper bound of the win.


```haskell
run :: (Int, Int) -> Int
run (t, rec) = ub - lb
  where
    beat n = n * (t - n) > rec
    Just lb = binaryMinSearch beat 1 t
    Just ub = binaryMinSearch (not . beat) lb t
```

Part 1 is to map `run` over the input and take the product. Part 2 requires
some massaging of the input, but then only a single iteration of `run` to find
the answer.

```haskell
  print . product $ map run input
  print . run . both (read . concatMap show) $ unzip input
```
