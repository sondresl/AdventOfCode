module Day24 where

import Lib
import Advent.Coord
import Advent.Parsers
import Data.Maybe
import Data.Bits (xor, (.|.), (.&.))
import Data.Digits
import Control.Monad
import Control.Monad.State
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import Text.RawString.QQ
import Text.ParserCombinators.Parsec hiding (count)
import Data.Tree
import Data.Foldable (toList)

data Op = And | Or | Xor
  deriving (Show, Eq, Ord)

data Gate = Gate Op String String String
  deriving (Show, Eq, Ord)

toOp :: Op -> (Int -> Int -> Int)
toOp = \case
  And -> (.&.)
  Or  -> (.|.)
  Xor -> xor

runGates :: Map String Int -> [Gate] -> Map String Int
runGates input gates = input'
  where
    input' = foldr f input gates
    f (Gate op left right output) acc = Map.insert output (toOp op l r) acc
        where 
          l = input' Map.! left
          r = input' Map.! right

-- Very useful for looking at the connections to a given node
ancestors :: [Gate] -> String -> Tree String
ancestors gates z1 = unfoldTree (\k -> (k <> " " <> op k, f k)) z1
  where
    op p = fromMaybe "<>" $ listToMaybe [ show o | (Gate o left right output) <- gates, output == p ]
    f p = case [ [left, right] | (Gate _ left right output) <- gates, output == p ] of
            [] -> []
            [xs] -> xs

-- Find all gates that output to a zXX with the wrong
-- operation. They should all be `XOR`s.
badGates = filter f
  where
    f (Gate op left right output) = head output == 'z' 
                                 && op /= Xor && output /= "z45"

-- Swap the output of two gates
swapGates :: (String, String) -> [Gate] -> [Gate]
swapGates (a, b) = foldr f []
  where
    f g@(Gate op left right output) acc
      | output == a = (Gate op left right b) : acc
      | output == b = (Gate op left right a) : acc
      | otherwise = g : acc
  
main :: IO ()
main = do
  (mp, gates) <- parseInput <$> readFile "../data/day24.in"
  -- Part 1
  let result = runGates mp gates
      zz = reverse . sort . filter ((== 'z') . head) $ Map.keys result
  print $ unDigits 2 $ map (result Map.!) zz
  let gates' = foldr swapGates gates [("z16", "hmk"), ("z33", "fcd"), ("z20", "fhp"), ("tpc", "rvf")]
      p2 = runGates mp gates'
      wrong = map (p2 Map.!) zz
  putStrLn $ intercalate "," $ sort ["z16", "hmk", "z33", "fcd", "z20", "fhp", "tpc", "rvf"]

parseInput :: String -> (Map String Int, [Gate])
parseInput input = (top', bot')
  where
    [top, bot] = splitOn "\n\n" input
    runParse p = either (error . show) id . traverse (parse p "") . lines
    top' = Map.fromList $ runParse pTop top
    pTop = do
      name <- replicateM 3 anyChar <* string ": "
      val <- pNumber
      pure $ (name, val)
    bot' = flip map (map words $ lines bot) $ \[left, op, right, "->", output] ->
      case op of
        "AND" -> Gate And left right output
        "XOR" -> Gate Xor left right output
        "OR"  -> Gate Or left right output 

-- 66055249060558
-- fcd,fhp,hmk,rvf,tpc,z16,z20,z33
