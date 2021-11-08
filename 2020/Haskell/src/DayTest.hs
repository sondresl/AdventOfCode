module DayTest where

sumOrOdd :: [Int] -> Either Int Int
sumOrOdd = foldMon f 0
  where
    f acc n
      | odd n = Left n
      | otherwise = Right $ acc + n

sumEven :: [Int] -> Maybe Int
sumEven = foldMon f 0
  where
    f acc n
      | odd n = Nothing
      | otherwise = Just $ acc + n

squareEven :: [Integer] -> [Integer]
squareEven = foldMon f 0
  where
    f acc n
      | odd n = [acc + n]
      | otherwise = [acc + n^2, acc + n]

foldMon :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldMon _ def [] = pure def
foldMon f def (x:xs) = do
  v <- f def x
  foldMon f v xs


main :: IO ()
main = do
  -- Maybe
  print $ sumEven [2,4..50]
  print $ sumEven $ [2,4..50] ++ [17] ++ [2,4..50]

  putStrLn "\n ------- \n"

  -- Either
  print $ sumOrOdd [2,4..50]
  print $ sumOrOdd $ [2,4..50] ++ [17] ++ [2,4..50]

  putStrLn "\n ------- \n"

  print $ squareEven [2,4..6]
  print $ squareEven [0..6]

