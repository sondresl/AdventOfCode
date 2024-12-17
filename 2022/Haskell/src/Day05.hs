module Day05 where

import Data.List.Extra (transpose, splitOn)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

type Stacks = IntMap String
type Cmd = (Int, Int, Int)

main :: IO ()
main = do
  (stack, cmds) <- parseInput <$> readFile "../data/day05.in"
  let score = map head . IM.elems
  putStrLn . score $ foldl (rearrange reverse) stack cmds
  putStrLn . score $ foldl (rearrange id     ) stack cmds

rearrange :: (String -> String) -> Stacks -> Cmd -> Stacks
rearrange f stack (n, from, to) = IM.adjust (new <>) to $ IM.adjust (drop n) from stack
  where new = f . take n $ stack IM.! from

parseInput :: String -> (Stacks, [Cmd])
parseInput input = (out top, cmds bottom)
  where
    (map lines -> [top, bottom]) = splitOn "\n\n" input
    cmds = map (parse . words)
    parse ["move", read -> n, "from", read -> from, "to", read -> to] = (n, from, to)
    parse e = error ("Bad input: " <> show e)
    out = foldMap f
        . filter (not . null) 
        . map (dropWhile (`notElem` "123456789")) 
        . transpose 
        . reverse 
    f (x:xs) = IM.singleton (read [x]) (reverse $ filter (/= ' ') xs)
    f e = error ("Bad input: " <> show e)

-- MQSHJMWNH
-- LLWJRBHVZ
