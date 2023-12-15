module Day15 where

import Lib (tuple)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (ord)

score :: Int -> Char -> Int
score acc = (`rem` 256) .(*17) . (+ acc) . ord

replaceTup xs (lab, val) =
  case break ((== lab) . fst) xs of
    (before, []) -> before <> [(lab, read val)]
    (before, _:after) -> before <> [(lab, read val)] <> after

removeTup :: [(String, Int)] -> String -> [(String, Int)]
removeTup xs lab = filter ((/= lab) . fst) xs

labels :: Map Int [(String, Int)] -> String -> Map Int [(String, Int)]
labels mp x
  | '=' `elem` x = let (lab, val) = tuple $ splitOn "=" x 
                       sc = foldl score 0 lab
                    in case Map.lookup sc mp of
                         Nothing -> Map.insert sc [(lab, read val)] mp
                         Just v -> Map.insert sc (replaceTup v (lab, val)) mp
  | '-' `elem` x = let (lab, _) = tuple $ splitOn "-" x 
                       sc = foldl score 0 lab
                    in case Map.lookup sc mp of
                         Nothing -> mp
                         Just v -> Map.insert sc (removeTup v lab) mp

scoreMap :: Map Int [(String, Int)] -> Int
scoreMap mp = sum $ do
  (box, xs) <- Map.assocs mp
  (slot, lens) <- zip [1..] (map snd xs)
  pure $ succ box * slot * lens

main :: IO ()
main = do
  input <- splitOn "," . init <$> readFile "../data/day15.in"
  print $ sum . map (foldl score 0) $ input
  print $ scoreMap $ foldl labels Map.empty input

-- 507291
-- 296921
