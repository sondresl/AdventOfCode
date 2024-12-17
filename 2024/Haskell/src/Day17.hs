module Day17 where

import Lib (allNums, fixedPoint)
import Data.Bits (xor)
import Control.Monad
import Data.List.Extra (tails, splitOn, intercalate, isSuffixOf)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

data Computer = Computer
 { instr :: IntMap Int
 , ip :: Int
 , a :: Int
 , b :: Int
 , c :: Int
 , out :: [Int]
 } deriving (Show, Eq, Ord)

compute :: Computer -> Computer
compute comp@(Computer instr ip a b c out) = case IM.lookup ip instr of
    Just 0 -> Computer instr (ip + 2) (a `div` 2^combo) b c out
    Just 1 -> Computer instr (ip + 2) a (b `xor` literal) c out
    Just 2 -> Computer instr (ip + 2) a (combo `mod` 8) c out
    Just 3 -> case a of
      0 -> Computer instr (ip + 2) a b c out
      _ -> Computer instr literal a b c out
    Just 4 -> Computer instr (ip + 2) a (b `xor` c) c out
    Just 5 -> Computer instr (ip + 2) a b c (combo `mod` 8 : out)
    Just 6 -> Computer instr (ip + 2) a (a `div` 2^combo) c out
    Just 7 -> Computer instr (ip + 2) a b (a `div` 2^combo) out
    Nothing -> comp
  where
    literal = instr IM.! (ip + 1)
    combo = case instr IM.! (ip + 1) of
      4 -> a
      5 -> b
      6 -> c
      n -> n
 
findStart :: Computer -> Int
findStart computer = minimum $ foldM go 0 original
  where
    original = reverse . map snd $ IM.toAscList (instr computer)
    go ((*8) -> acc) n = do
      v <- [0..7]
      let output = getOutput (computer { a = acc + v })
      guard $ head output == n
      pure $ (acc + v)

getOutput :: Computer -> [Int]
getOutput input = reverse $ out comp
  where Just comp = fixedPoint compute input

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day17.in"
  let comp = getOutput input
  putStrLn . intercalate "," $ map show comp
  print $ findStart input

parseInput :: String -> Computer
parseInput input = computer
  where
    [top, bot] = splitOn "\n\n" input
    prog = IM.fromList . zip [0..] $ allNums bot
    computer = let [a,b,c] = allNums top in Computer prog 0 a b c []

-- 7,1,5,2,4,0,7,6,1
-- 37222273957364
