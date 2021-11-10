module Fixx where

import Control.Lens

_Factor :: (Choice p, Applicative f, Integral a) => a -> p a (f a) -> p a (f a)
_Factor n = prism' embed match
  where
    embed i = i * n
    match i = case i `mod` n of
                0 -> Just (i `div` n)
                _ -> Nothing

prismFizzBuzz :: Int -> String
prismFizzBuzz n
  | has (_Factor 3 . _Factor 5) n = "FizzBuzz"
  | has (_Factor 3) n = "Fizz"
  | has (_Factor 5) n = "Buzz"
  | otherwise = show n

_Conss :: Prism [a] [b] (a, [a]) (b, [b])
_Conss = prism embed match
  where
    match [] = Left []
    match (x:xs) = Right (x, xs)
    embed (x, xs) = x : xs

main :: IO ()
main = do
  mapM_ (putStrLn . prismFizzBuzz) [1..20]
  print $ review _Conss (0, [1..10])
  print $ toListOf (worded . (_Show :: Prism' String Int)) "1 2 3 4 56"
