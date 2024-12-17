module Day19 where

import Lib (dfs, allNums)
import Control.Lens (view, _4)
import Control.Monad (guard)
import Data.List.Extra (nub)
import Data.Map (Map)
import qualified Data.Map as Map

type Blueprint = (Int,Int,(Int,Int),(Int,Int))
data Material = Ore | Clay | Obsidian | Geode deriving (Show, Eq, Ord)

data Log = Log
  { bp :: Int
  , ore :: Int
  , clay :: Int
  , obsidian :: Int
  , geode :: Int
  , oreR :: Int
  , clayR :: Int
  , obsR :: Int
  , geodeR :: Int
  , steps :: Int
  , skipped :: [Material]
  } deriving (Show, Eq, Ord)

compute :: Int -> Log -> Blueprint -> [Log]
compute count start blueprint = dfs project next start
  where
    project (Log bp ore clay obsidian geode oreR clayR obsR geodeR n skipped) = (clay, obsidian, geode, oreR, clayR, obsR, geodeR, n)
    next (Log bp ore clay obs geo oreR clayR obsR geodeR n skipped) = do
         (oreR', clayR', obsR', geodeR', ore', clay', obs', skipped') <- construct blueprint ore clay obs skipped
         guard $ n <= count
         pure (Log bp (ore' + oreR) (clay' + clayR) (obs' + obsR) (geo + geodeR)
                      (oreR + oreR') (clayR + clayR') (obsR + obsR') (geodeR + geodeR') (1 + n)
                      skipped')
     where
       construct :: Blueprint -> Int -> Int -> Int -> [Material] -> [(Int, Int, Int, Int, Int, Int, Int, [Material])]
       construct (oreOre, clayOre, (obsidianOre, obsidianClay), (geodeOre, geodeObsidian)) ore clay obsidian skipped =
         let xs = [ (0,0,0,1,ore - geodeOre, clay, obsidian - geodeObsidian, [])
                    | ore >= geodeOre && obsidian >= geodeObsidian
                  ]
               <> [ (0,0,1,0,ore - obsidianOre, clay - obsidianClay, obsidian, [])
                    | ore >= obsidianOre && clay >= obsidianClay, Obsidian `notElem` skipped, obsR <= geodeObsidian
                  ]
               <> [ (0,1,0,0, ore - clayOre, clay, obsidian, [])
                    | ore >= clayOre, Clay `notElem` skipped, clayR <= obsidianClay
                  ]
               <> [ (1,0,0,0,ore - oreOre, clay, obsidian, [])
                    | ore >= oreOre, Ore `notElem` skipped, oreR <= maximum [oreOre, clayOre, obsidianOre ]
                  ]
               <> [ (0,0,0,0, ore, clay, obsidian, skipped') ]
          in if not (null xs) && view _4 (head xs) == 1 then take 1 xs else xs
         where skipped' = nub ([ Obsidian | ore >= obsidianOre && clay >= obsidianClay ] 
                            <> [ Clay | ore >= clayOre ] 
                            <> [ Ore | ore >= oreOre ] 
                            <> skipped)



main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day19.in"
  let run n xs = map (filter ((==n) . steps) . compute n (Log 1 0 0 0 0 1 0 0 0 0 []) . (input Map.!)) $ xs
  print . sum . zipWith (*) [1..] . map (maximum . map geode) . run 24          $ Map.keys input
  print . product                 . map (maximum . map geode) . run 32 . take 3 $ Map.keys input

parseInput :: String -> Map Int Blueprint
parseInput = Map.fromList . map (f . allNums) . lines
  where f [id, ore, clay, obsidianOre, obsidianClay, geodeOre, geodeObsidian]
          = (id, (ore, clay, (obsidianOre, obsidianClay), (geodeOre, geodeObsidian)))

-- 960
-- 2040
