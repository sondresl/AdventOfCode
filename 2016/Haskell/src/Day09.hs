{-# LANGUAGE TypeApplications #-}
module Day09 where

import Control.Lens
import Lib
import Data.List.Extra
import qualified Text.Parsec as P
import Text.Parsec (anyToken, letter, count, oneOf, satisfy, char, many1, many, digit)
import qualified Data.Map as Map
import Control.Monad

type Parser = P.Parsec String ()

parseInput = init

run :: Bool -> String -> Int
run recurse = either (error . show) id . P.parse (decompress recurse) ""

decompress :: Bool -> Parser Int
decompress recurse = (sum <$>) . many $ inner
  where 
    inner = do
      c <- anyToken
      case c of
        '(' -> do
          n <- read @Int <$> many1 digit
          char 'x'
          m <- read @Int <$> many1 digit
          char ')'
          str <- replicateM n anyToken
          pure . (* m) $ if recurse
             then run recurse str
             else length str
        _ -> (+1) . length <$> many letter

part1 :: String -> Int
part1 = run False

part2 :: String -> Int
part2 = run True

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        putStrLn ("\nInput file: " ++ show file ++ "\n")
        print $ part1 input
        print $ part2 input

  run "../data/day09.in"

-- 
-- 
