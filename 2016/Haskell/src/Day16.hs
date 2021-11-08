{-# LANGUAGE OverloadedStrings #-}
module Day16 where

import Control.Lens
import Lib
import Data.List.Extra (chunksOf)
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Data.Bool (bool)


solve :: Int -> [Bool] -> [Bool]
solve len = checksum . take len . until ((>= len) . length) genData
  where
  genData a = a <> [False] <> map not (reverse a)
  checksum = until (odd . length) run
    where
      run = map (foldl1 (==)) . chunksOf 2

part1 :: Int -> [Bool] -> String
part1 n = map (bool '0' '1') . solve n

part2 :: Int -> [Bool] -> String
part2 n = map (bool '0' '1') . solve n

toRep :: Bool -> Char
toRep a = if a then '1' else '0'

main :: IO ()
main = do
  let run str = do
        putStrLn str
        let input = map (== '1') "01110110101001000"
        print $ part1 272 input
        print $ part2 35651584 input

  run "\nActual:\n"

-- "11100111011101111"
-- "10001110010000110" (~100 s)
