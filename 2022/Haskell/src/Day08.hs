module Day08 where

import Data.List.Extra (transpose, chunksOf, nubOrdOn)

findGrowing :: [(Int, (Int, Int))] -> [(Int, (Int, Int))]
findGrowing xs = fromLeft xs <> fromRight xs
  where
    fromLeft (x:xs) = x : go x xs
    fromLeft _ = error "from left"
    fromRight (reverse -> (x:xs)) = x : go x xs
    fromRight _ = error "from right"
    go c [] = []
    go (c, ic) ((x, ix):xs)
      | x > c = (x, ix) : go (x, ix) xs
      | otherwise = go (c, ic) xs

addIx :: [[Int]] -> [(Int, (Int, Int))]
addIx xs = do
  (lx, ls) <- zip [1..] xs 
  (ix, x) <- zip [1..] ls
  pure (x, (lx, ix))

fromTree :: [(Int, (Int, Int))] -> [Int]
fromTree (map fst -> xs) = f [] xs
  where
    f [] (x:xs) = 0 : f [x] xs
    f ls [x] = [0]
    f ls (x:rs) = let len = findLen x ls * findLen x rs
                   in len : f (x:ls) rs
    f _ _ = error "f"
    findLen orig [x] = 1
    findLen orig (x:y:xs)
      | x >= orig = 1
      | otherwise = 1 + findLen orig (y:xs)
    findLen _ e = error "findLen"

part2 :: [[(Int, (Int, Int))]] -> Int
part2 input = maximum $ zipWith (*) (concat hori) (concat $ transpose . transpose . transpose $ vert)
  where
    hori = map fromTree input
    vert = map fromTree $ transpose input

main :: IO ()
main = do
  input' <- map (map (read @Int . (:[]))). lines <$> readFile "../data/day08.in"
  let input = chunksOf (length (head input')) $ addIx input'
      angles xs = xs <> transpose xs
  print . length . nubOrdOn snd . concatMap findGrowing . angles $ input
  print $ part2 input
    
-- 1647
-- 392080
