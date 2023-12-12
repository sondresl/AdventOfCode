module Day12 where

import Lib
import Advent.Coord
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count, Parser)
import qualified Text.Parsec as P
import Data.Functor (($>))

import Debug.Trace
import Data.Either (rights)
import Data.MemoTrie (memo, memo2)

part1 input = undefined

part2 input = undefined

type Parser = P.ParsecT String [Int] []

getNum :: Parser Int
getNum = do
  ns <- P.getState
  guard $ not (null ns)
  P.modifyState tail
  pure $ head ns

parseNumQ :: Int -> Int -> Parser String
parseNumQ d n = do
  pre <- map (const '.') <$> replicateM d (oneOf ".?") -- d dots
  res <- map (const '#') <$> replicateM n (oneOf "?#") -- n #s
  end <- (P.eof $> '|') <|> (P.oneOf ".?" $> '.')
  ((pre <> res <> pure end) <>) <$> parseString

tryParse :: Parser String
tryParse = do
  P.skipMany (char '.')
  n <- getNum
  d <- lift [0..15 * 5]
  -- P.parserTrace ("LABEL " <> show (n, d))
  P.try (parseNumQ d n)

parseString :: Parser String
parseString = P.try finished <|> tryParse
  where
    finished = do
      P.skipMany (oneOf ".?")
      P.eof
      xs <- P.getState
      guard $ null xs
      -- P.parserTrace "SUCCESS"
      pure []

go str ints = length . rights $ P.runParserT tryParse ints "" str

go' str ints = length . rights $ P.runParserT tryParse ints' "" str'
  where 
    ints' = concat $ replicate 5 ints
    str' = intercalate "?" $ replicate 5 str

main :: IO ()
main = do

  let run str input = do
        putStrLn str
        -- mapM_ print input
        let (str, ints) = head input
        let res = map (uncurry go') input
        mapM_ print res
        print $ sum res

        -- print $ part1 input
        -- print $ part2 input

  run "\nTest:\n\n" $ parseInput testInput

  -- input <- parseInput <$> readFile "../data/day12.in"
  -- run "\nActual:\n\n" input

parseInput :: String -> [(String, [Int])]
parseInput = map (f . tuple . words) . lines
  where
    f (str, nums) = (str, allNums nums)

-- parseInput = either (error . show) id . traverse (parse p "") . lines
--   where
--     p = undefined

testInput = [r|???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
|]
