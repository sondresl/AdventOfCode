{-# LANGUAGE TupleSections #-}
module Day24 where

import Control.Lens
import Lib
import Data.List.Extra
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

type Floor = Maybe Int

parseInput :: String -> Map Point Floor
parseInput = parseAsciiMap f
  where
    f c
      | c `elem` "0123456789" = Just $ Just (read @Int $ pure c)
      | c == '.' = Just Nothing
      | otherwise = Nothing

run :: Map Point Floor -> [(Point, [Int], Int)]
run points = 
  let allKeys = mapMaybe snd $ Map.toList points
      start = map ((,allKeys, 0) . fst) . filter ((== Just 0) . snd) $ Map.toList points 
      f (pos, keys, steps) = 
        let next = filter (`elem` map fst (Map.toList points)) $ neighbours4 pos
            keys' = case Map.lookup pos points of
                      Just Nothing -> keys
                      Just (Just v) -> delete v keys
                      Nothing -> keys
         in map (,keys', steps + 1) next
   in bfs start f


part1 :: Map Point Floor -> Maybe Int
part1 = over _Just (subtract 1 . view _3) . find (null . view _2) . run

part2 = undefined

main :: IO ()
main = do
  let run file = do
        input <- parseInput <$> readFile file
        putStrLn ("\nInput file: " ++ show file ++ "\n")
        -- print input
        print $ part1 input
        -- print $ part2 input

  run "../data/test"
  run "../data/day24.in"

-- 
-- 
