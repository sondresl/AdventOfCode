module Day25 where

fromSnafu :: Char -> Int
fromSnafu = \case
  '=' -> -2
  '-' -> -1
  '0' -> 0
  '1' -> 1
  '2' -> 2

toSnafu :: Int -> Char
toSnafu = \case
  (-2) -> '=' 
  (-1) -> '-' 
  0    -> '0' 
  1    -> '1' 
  2    -> '2' 

dMod :: Int -> (Int, Int) -- (value, rest)
dMod n 
  | n <= -3   = (n + 5, -1)
  | n >=  3   = (n - 5,  1)
  | otherwise = (n     , 0)

addSnafu :: [Int] -> [Int] -> [Int]
addSnafu a b = go 0 (combine a b)
  where
    go mente [] = if mente == 0 then [] else [mente]
    go mente (x:xs) = let (y, mente') = dMod (x + mente) in y : go mente' xs
    combine [] ys = ys
    combine xs [] = xs
    combine (x:xs) (y:ys) = (x + y) : combine xs ys

main :: IO ()
main = do
  input <- map (map fromSnafu . reverse) . lines <$> readFile "../data/day25.in"
  putStrLn . map toSnafu . reverse $ foldr1 addSnafu input

-- 2-20=01--0=0=0=2-120
