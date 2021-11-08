module Day02 where

import Control.Lens
import Data.Maybe (fromJust)
import Linear (V2 (..))

parseInput :: String -> [[Pos]]
parseInput = map (map toDir) . lines
  where
    toDir :: Char -> V2 Int
    toDir 'L' = V2 (-1) 0
    toDir 'R' = V2 1 0
    toDir 'D' = V2 0 1
    toDir 'U' = V2 0 (-1)
    toDir _ = error "Unexpected character"

type Keypad = [String]

type Pos = V2 Int

(!!?) :: [[a]] -> Pos -> Maybe a
(!!?) kp (V2 x y) = (kp ^? ix y) >>= (^? ix x)

(!!!) :: [[a]] -> Pos -> a
(!!!) kp (V2 x y) = fromJust ((kp ^? ix y) >>= (^? ix x))

newPos :: Keypad -> Pos -> Pos -> Pos
newPos kp old new =
  case kp !!? new of
    Nothing -> old
    Just '0' -> old
    _ -> new

move :: Keypad -> Pos -> [[Pos]] -> String
move kp = go
  where 
    go pos [] = []
    go pos ([] : xs) = kp !!! pos : move kp pos xs
    go pos ((dir : ls) : xs) = move kp (newPos kp pos (pos + dir)) (ls : xs)

part1 :: [[Pos]] -> String
part1 = move keypad (V2 1 1)
  where
    keypad :: Keypad
    keypad =
      [ "123",
        "456",
        "789"
      ]

part2 :: [[Pos]] -> String
part2 = move keypad (V2 0 2)
  where
    keypad :: Keypad
    keypad =
      [ "00100",
        "02340",
        "56789",
        "0ABC0",
        "00D00"
      ]

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        putStrLn ("\nInput file: " ++ show file)
        putStrLn $ part1 input
        putStrLn $ part2 input

  run "../data/day02.in"

-- 19636
-- 3CC43
