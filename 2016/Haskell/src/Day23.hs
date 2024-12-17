{-# LANGUAGE RecordWildCards #-}
module Day23 where

import Control.Lens
import Lib
import Data.List.Extra
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Vector as V
import Data.Functor.Foldable
import Text.Pretty.Simple (pPrint)

type Memory = Map Char Int

data Computer = Computer 
  { mem :: Memory
  , ip :: Int
  , ins :: V.Vector Instruction
  } deriving Show

type Value = Either Char Int

data Instruction
  = Cpy Value Value
  | Inc Value
  | Dec Value
  | Jnz Value Value
  | Tgl Value
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
      Inc (Left reg) -> c { mem = Map.adjust (+1) reg mem, ip = ip + 1 } 
      Inc (Right _) -> c { ip = ip + 1 } 
      Dec (Left reg) -> c { mem = Map.adjust (subtract 1) reg mem, ip = ip + 1 } 
      Dec (Right _) -> c { ip = ip + 1 } 
      Cpy val (Left reg) -> c { mem = Map.insert reg (getVal val mem) mem, ip = ip + 1 }
      Cpy val (Right _) -> c { ip = ip + 1 }
      Jnz val reg -> c { ip = if getVal val mem == 0 then ip + 1 else ip + getVal reg mem }
      Tgl val -> 
        let ix = getVal val mem + ip
            old = ins V.! ix
            new = case old of
                    Inc x -> Dec x
                    Dec x -> Inc x
                    Tgl x -> Inc x
                    Jnz x y -> Cpy x y
                    Cpy x y -> Jnz x y
         in if ix >= V.length ins
               then c { ip = ip + 1 }
               else c { ins = ins V.// [(ix, new)], ip = ip + 1 }

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
        let emptyMem = Map.insert 'a' 7 $ Map.fromList $ zip "abcd" (repeat 0)
            comp = Computer emptyMem 0 input
        putStrLn ("\nInput file: " ++ show file ++ "\n")
        mapM_ print input
        putStrLn ""
        pPrint . (!! 5) . iterate step $ comp
        print $ part1 comp
        -- print $ part2 comp


  run "../data/test"
  run "../data/day23.in"

parseInput :: String -> V.Vector Instruction
parseInput = V.fromList . map (parseIns . words) . lines
  where
    getValue str = if all (`elem` "-0123456789") str
                      then Right (read str)
                      else Left (head str)
    parseIns :: [String] -> Instruction
    parseIns ["cpy", a, b] = Cpy (getValue a) (getValue b)
    parseIns ["inc", a] = Inc $ getValue a
    parseIns ["dec", a] = Dec $ getValue a
    parseIns ["jnz", a, b] = Jnz (getValue a) (getValue b)
    parseIns ["tgl", a] = Tgl $ getValue a
    parseIns e = error ("Bad input: " <> show e)
