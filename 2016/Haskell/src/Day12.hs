{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module Day12 where

import Control.Lens
import Lib
import Data.List.Extra
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Vector as V
import Data.Functor.Foldable

type Memory = Map Char Int

data Computer = Computer 
  { mem :: Memory
  , ip :: Int
  , ins :: V.Vector Instruction
  } deriving Show

type Value = Either Char Int

data Instruction
  = Cpy Value Char
  | Inc Char
  | Dec Char
  | Jnz Value Int
  deriving Show

data Log a
  = Halt Int
  | Run a
  deriving (Show, Functor)

getVal :: Value -> Memory -> Int
getVal (Left c) mem = mem Map.! c
getVal (Right i) _ = i

step :: Computer -> Computer
step c@(Computer mem ip ins)
  = case ins V.! ip of
      Inc reg -> c { mem = Map.adjust (+1) reg mem, ip = ip + 1 } 
      Dec reg -> c { mem = Map.adjust (subtract 1) reg mem, ip = ip + 1 } 
      Cpy val reg -> c { mem = Map.insert reg (getVal val mem) mem, ip = ip + 1 }
      Jnz val reg -> c { ip = if getVal val mem == 0 then ip + 1 else ip + reg }

run :: Computer -> Log Computer
run c@Computer {..} = if ip >= V.length ins
                then Halt $ mem Map.! 'a'
                else Run $ step c

part1 :: Computer -> Int
part1 = getResult `hylo` run
  where
    getResult :: Log Int -> Int
    getResult (Halt i) = i
    getResult (Run c) = c

part2 = undefined

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        let emptyMem = Map.fromList $ zip "abcd" (repeat 0)
            comp = Computer emptyMem 0 input
        putStrLn ("\nInput file: " ++ show file ++ "\n")
        mapM_ print input
        putStrLn ""
        print $ part1 comp
        -- print $ part2 input

  run "../data/test"
  run "../data/day12.in"

-- 409147
-- Just 991

parseInput :: String -> V.Vector Instruction
parseInput = V.fromList . map (parseIns . words) . lines
  where
    getValue str = if all (`elem` "0123456789") str
                      then Right (read str)
                      else Left (head str)
    parseIns :: [String] -> Instruction
    parseIns ["cpy", a, b] = Cpy (getValue a) (head b)
    parseIns ["inc", a] = Inc $ head a
    parseIns ["dec", a] = Dec $ head a
    parseIns ["jnz", a, b] = Jnz (getValue a) (read b)
