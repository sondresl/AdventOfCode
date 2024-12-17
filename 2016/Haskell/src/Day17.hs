module Day17 where

import Control.Lens
import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Data.List.Extra
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lib
import Linear
import Text.ParserCombinators.Parsec

generate :: String -> String
generate = unpack . BS.take 4 . digestToHexByteString . md5 . pack
  where
    md5 :: ByteString -> Digest MD5
    md5 = hash

move :: String -> (Point, String) -> [(Point, String)]
move salt (V2 3 0, path) = []
move salt (pos, path) =
  let [u', d', l', r'] = map (`elem` "bcdef") . take 4 $ generate (salt <> path)
      u = (u', (pos + V2 0 1, path ++ "U"))
      d = (d', (pos + V2 0 (-1), path ++ "D"))
      r = (r', (pos + V2 1 0, path ++ "R"))
      l = (l', (pos + V2 (-1) 0, path ++ "L"))
   in map snd $ filter fst $ filter (inBounds . fst . snd) [u, d, l, r]
  where
    inBounds (V2 x y) = x >= 0 && y >= 0 && x <= 3 && y <= 3

part1 :: String -> Maybe String
part1 salt = lookup (V2 3 0) $ bfs [(V2 0 3, "")] (move salt)

part2 :: String -> Int
part2 salt =
  length
    . snd
    . last
    . filter ((== V2 3 0) . fst)
    $ bfs [(V2 0 3, "")] (move salt)

main :: IO ()
main = do
  let run str code = do
        print $ part1 code
        print $ part2 code

  run "\nActual:\n" "ioramepc"

-- 409147
-- Just 991
