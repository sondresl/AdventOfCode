{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack, index)
import Data.List.Extra (sortOn)
import qualified Data.Text as T
import Numeric (showHex)
import Text.Printf
import Data.Char
import Data.List (delete)

testId :: ByteString
testId = "abc"

doorId :: ByteString
doorId = "wtnhxymk"

generate :: [ByteString]
generate = map f [1 ..]
  where
    md5 :: ByteString -> Digest MD5
    md5 = hash
    f :: Int -> ByteString
    f seed = digestToHexByteString . md5 $ doorId <> pack (show seed)

    -- f seed = B.foldr (\b -> (<>) (T.pack $ printf "%02x" b)) "" . hash $ doorId <> pack (show seed)

part1 :: [ByteString] -> String
part1 = map (`index` 5) . take 8

part2 :: [ByteString] -> String
part2 =
  map (`index` 6)
    . sortOn (`index` 5)
    . take 8
    . getFirst8

getFirst8 :: [ByteString] -> [ByteString]
getFirst8 = go ("01234567" :: String)
  where
    go :: String -> [ByteString] -> [ByteString]
    go "" _ = []
    go missing (x:xs) = if index x 5 `elem` missing
                           then x : go (delete (index x 5) missing) xs
                           else go missing xs

main :: IO ()
main = do
  let run file = do
        let input = doorId
        putStrLn ("\nInput file: " ++ show file ++ "\n")
        let hashes = filter (B.isPrefixOf "00000") generate
        mapM_ print $ take 8 hashes
        putStrLn $ part1 hashes
        putStrLn $ part2 hashes

  run doorId

-- "2414bc77"
-- "437e60fc"
