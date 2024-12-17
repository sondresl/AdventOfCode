module Day18 where

import Control.Lens
import Lib
import Data.List.Extra
import qualified Data.Map as Map

parseInput = init

sliding :: Int -> [a] -> [[a]]
sliding n str = filter ((== n) . length) . map (take n) $ tails str

gen :: String -> String
gen str = map next . sliding 3 $ "." <> str <> "."
  where
    next "^^." = '^'
    next ".^^" = '^'
    next "^.." = '^'
    next "..^" = '^'
    next _ = '.'

runN :: Int -> String -> Int
runN n = count (== '.') . concat . take n . iterate gen

part1 :: String -> Int
part1 = runN 40

part2 :: String -> Int
part2 = runN 400000

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        print $ part1 input
        print $ part2 input

  run "../data/day18.in"

-- 2035
-- 20000577
