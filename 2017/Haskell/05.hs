{-# LANGUAGE TypeApplications #-}
module Main where

import Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as PL
import Control.Lens
import Control.Monad.Extra

type Tape = PointedList Int

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a = a : case f a of 
                         Nothing -> []
                         Just v -> iterateMaybe f v


step :: (Int -> Int) -> Tape -> Maybe Tape
step f tape = PL.moveN curr $ over PL.focus f tape
  where
    curr = view PL.focus tape

part1 :: Tape -> Int
part1 = length . iterateMaybe (step (+1))

part2 :: Tape -> Int
part2 = length . iterateMaybe (step (\x -> if x >= 3 then (x - 1) else (x + 1)))

main :: IO ()
main = do
  Just input <- PL.fromList . map (read @Int) . lines <$> readFile "../data/05.in"
  print $ part1 input
  print $ part2 input
