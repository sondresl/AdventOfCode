{-# LANGUAGE ViewPatterns #-}
module Day21 where

import Debug.Trace

import Control.Lens
import Lib
import Data.List.Extra
import Data.Foldable (toList)
import Text.ParserCombinators.Parsec
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (replicateM)

data Instruction
  = SwapIx Int Int
  | SwapCh Char Char
  | RotateL Int
  | RotateR Int
  | RotateCh Char
  | Reverse Int Int
  | Move Int Int
  deriving (Show, Eq, Ord)

type Code = Seq Char

runIns :: Code -> Instruction -> Code
runIns code (SwapIx x y) =
  let x' = Seq.index code x
      y' = Seq.index code y
   in Seq.adjust' (const x') y $ Seq.adjust' (const y') x code
runIns code (SwapCh x y) = fmap f code
  where
    f ch 
      | ch == x = y
      | ch == y = x
      | otherwise = ch
runIns code (RotateL y) = iterateN y rotateL code
  where
    rotateL (x :< rest) = rest |> x
    rotateL _ = error ""
runIns code (RotateR y) = iterateN y rotateR code
  where
    rotateR (rest :> x) = x <| rest
    rotateR _ = error ""
runIns code (RotateCh y) = 
  let Just ix = Seq.findIndexL (== y) code
   in runIns code (RotateR $ ix + (if ix >= 4 then 2 else 1))
runIns code (Reverse x y) = 
  let pre = Seq.take x code
      mid = Seq.reverse $ Seq.take (y - x + 1) $ Seq.drop x code
      end = Seq.drop (1 + y) code
   in pre <> mid <> end
runIns code (Move x y) = 
  let m = Seq.index code x
   in Seq.insertAt y m $ Seq.deleteAt x code

part1 :: String -> [Instruction] -> String
part1 (Seq.fromList -> start) = toList . foldl runIns start

part2 :: [Instruction] -> String
part2 ins = head $ filter ((== target) . (`part1` ins)) candidates
  where
    target = "fbgdceah"
    candidates = filter ((== 8) . length . nub) $ replicateM 8 "abcdefgh"

main :: IO ()
main = do
  let run file start = do
        input <- parseInput <$> readFile file
        print $ part1 start input
        print $ part2 input

  let testCode = Map.fromList $ zip [0..] "abcde"

  run "../data/day21.in" "abcdefgh"

-- "dbfgaehc"
-- "aghfcdeb"

parseInput :: String -> [Instruction]
parseInput = either (error . show) id . traverse (parse parseIns "") . lines
  where
    num = read @Int <$> many1 digit
    parseIns = try swapIx 
      <|> try swapCh 
      <|> try rotL 
      <|> try rotR 
      <|> try rotBased
      <|> try rev
      <|> try mov
    swapIx = SwapIx <$>
      (string "swap position " *> num) <*> (string " with position " *> num)
    swapCh = SwapCh <$> 
      (string "swap letter " *> letter) <*> (string " with letter " *> letter)
    rotL = RotateL <$> (string "rotate left " *> num)
    rotR = RotateR <$> (string "rotate right " *> num)
    rotBased = RotateCh <$> (string "rotate based on position of letter " *> letter)
    rev = Reverse <$> (string "reverse positions " *> num) <*> (string " through " *> num)
    mov = Move <$>
      (string "move position " *> num) <*> (string " to position " *> num)
