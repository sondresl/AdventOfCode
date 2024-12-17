module Day07 where

import Control.Lens
import Lib
import Data.List.Extra
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Control.Monad (guard)

type Address = (String, String, String)

type Addr = ([String], [String])

parseInput :: String -> [Addr]
parseInput = map parseAddr .  lines
  where
    parseAddr :: String -> Addr
    parseAddr xs = case span (/= '[') xs of
                     (xs, []) -> ([xs], [])
                     (xs, ys) -> let (inner, rest) = span (/= ']') (drop 1 ys)
                                  in ([xs], [inner]) <> parseAddr (drop 1 rest)

tls :: Eq a => [a] -> Bool
tls x = any (\x -> palindrome x && length (nub x) > 1) (slidingWindow 4 x)
  where
    palindrome = (==) <*> reverse

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n = filter ((== n) . length) . go
  where 
    go [] = []
    go xs = take n xs : go (drop 1 xs)

sls :: Addr -> Bool
sls (a, b) = or $ do
    c <- candidates
    b <- babs
    pure $ bab b c
  where
    aba [a,b,c] = a == c && a /= b
    bab [x,y,z] [a,b,c] = x == b && y == a && a == c
    babs = filter aba $ concatMap (slidingWindow 3) b
    candidates = filter aba $ concatMap (slidingWindow 3) a

part1 :: [Addr] -> Int
part1 = Lib.count valid
  where
    valid (a, b) = any tls a && not (any tls b)

part2 = Lib.count sls

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        putStrLn ("\nInput file: " ++ show file ++ "\n")
        print $ part1 input
        print $ part2 input

  run "../data/test"
  run "../data/day07.in"
