module Day13 where

import Lib (Point)
import Advent.Parsers (pNumber)
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.Parsec (sepEndBy, newline, parse, string, try)
import Linear (V2(V2))
import Data.SBV hiding (solve)

data Machine = Machine {
  buttonA :: Point,
  buttonB :: Point,
  prize :: Point
} deriving (Show, Eq)

solve :: Machine -> IO OptimizeResult
solve = optimize Independent . solve
    where
      solve (Machine (V2 ax ay) (V2 bx by) (V2 px py)) = do
        [sa, sb, total] <- sIntegers ["a", "b", "c"]
        constrain $ sa * literal (fromIntegral ax) + sb * literal (fromIntegral bx) .== literal (fromIntegral px)
        constrain $ literal (fromIntegral ay) * sa + literal (fromIntegral by) * sb .== literal (fromIntegral py)
        constrain $ total .== (sa * 3 + sb)
        minimize "Cost" total

runSolve :: [Machine] -> IO ()
runSolve ms = do
  results <- mapM solve ms
  let res = mapMaybe extract results
  print $ sum res
    where
      extract :: OptimizeResult -> Maybe Integer
      extract (IndependentResult [(_, result)]) = getModelValue "c" result

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day13.in"
  runSolve input
  let inc (Machine a b p) = Machine a b (p + 10000000000000)
  runSolve $ map inc input

parseInput :: String -> [Machine]
parseInput = either (error . show) id . parse p ""
  where
    p = machine `sepEndBy` (try $ newline *> newline)
    machine = do
      pa <- V2 <$> (string "Button A: X+" *> pNumber <* string ", Y+") <*> (pNumber <* newline)
      pb <- V2 <$> (string "Button B: X+" *> pNumber <* string ", Y+") <*> (pNumber <* newline)
      prize <- V2 <$> (string "Prize: X=" *> pNumber <* string ", Y=") <*> pNumber
      pure $ Machine pa pb prize

-- 39290
-- 73458657399094
