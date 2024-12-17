module Day24 where

import Data.SBV 
import Data.Char (isDigit)
import Data.List.Extra (chunksOf)
import Control.Lens (has, makePrisms)
import Data.Map (Map)
import qualified Data.Map as Map

data Instruction = A Integer | B Integer Integer
  deriving (Show, Eq, Ord)

step :: (String -> SInteger -> Symbolic ()) -> [Instruction] -> Symbolic SBool
step f = go (Map.singleton 'z' (literal 0))  0
    where
      go mp res [] = do
        f "Total" res
        pure $ mp Map.! 'z' .== 0
      go mp res (v:xs) = do
        w <- sInteger "w"
        constrain $ w .< 10 .&& w .> 0
        let res' = res * 10 + w
            z = mp Map.! 'z'
        case v of
          A x -> go (Map.insert 'z' (z * 26 + (literal x + w)) mp) res' xs
          B x y -> go (Map.insert 'z' (ite ((z `sMod` 26) + literal x .== w) 
                                            (z `sDiv` 26) 
                                           ((z `sDiv` 26) * 26 + (w + literal y))) 
                                      mp) res' xs

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day24.in"
  let run f = optimize Independent (step f input) >>= print
  run maximize
  run minimize
    
parseInput :: String -> [Instruction]
parseInput = map (f . map words) . filter (not . null) . chunksOf 18 . lines
  where
    f :: [[String]] -> Instruction
    f xs = if ["div", "z", "1"] `elem` xs
              then A (let ["add", _, x] = xs !! 15 in read x)
              else B (let ["add", _, x] = xs !! 5 in read x)  (let ["add", _, x] = xs !! 15 in read x)

-- 95299897999897
-- 31111121382151
