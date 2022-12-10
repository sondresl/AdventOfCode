module Day10 where

import Lib (display)
import Linear (V2(..))
import Data.Set (Set)
import Data.Bool (bool)
import qualified Data.Set as Set

data Op = AddX Int | NoOp
  deriving (Show, Eq, Ord)

compute :: (Int, Int) -> Op -> (Int, Int)
compute (cycle, reg) = \case 
  NoOp -> (succ cycle, reg)
  AddX v -> (succ cycle, reg + v)

lightCRT :: Set (V2 Int) -> (Int, Int) -> Set (V2 Int)
lightCRT pixels (cycle, val) = bool pixels (Set.insert (V2 col row) pixels) inSprite 
  where
    (row, col) = (cycle - 1) `divMod` 40
    inSprite = col `elem` [pred val, val, succ val]

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day10.in"
  let res = scanl compute (1,1) input
      lookupStrength ix = let Just b = lookup ix res in ix * b
  print $ sum $ map lookupStrength [20,60,100,140,180,220]
  putStrLn $ display $ foldl lightCRT Set.empty res

parseInput :: String -> [Op]
parseInput = concatMap (f . words) . lines
  where
    f ["noop"] = [NoOp]
    f ["addx", n] = [AddX 0, AddX (read n)]
    f _ = error "f"

-- 14560
-- ▓▓▓▓ ▓  ▓ ▓▓▓  ▓  ▓ ▓▓▓▓ ▓▓▓  ▓  ▓ ▓▓▓▓
-- ▓    ▓ ▓  ▓  ▓ ▓  ▓ ▓    ▓  ▓ ▓  ▓    ▓
-- ▓▓▓  ▓▓   ▓  ▓ ▓▓▓▓ ▓▓▓  ▓  ▓ ▓  ▓   ▓
-- ▓    ▓ ▓  ▓▓▓  ▓  ▓ ▓    ▓▓▓  ▓  ▓  ▓
-- ▓    ▓ ▓  ▓ ▓  ▓  ▓ ▓    ▓    ▓  ▓ ▓
-- ▓▓▓▓ ▓  ▓ ▓  ▓ ▓  ▓ ▓▓▓▓ ▓     ▓▓  ▓▓▓▓
