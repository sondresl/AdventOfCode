{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import Control.Monad (guard)
import Crypto.Hash
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Byteable
import Data.List
import Lib

genHash :: ByteString -> ByteString
genHash = digestToHexByteString . md5
  where
    md5 :: ByteString -> Digest MD5
    md5 = hash

run :: (ByteString -> ByteString) -> ByteString -> Int
run strat salt = fst . (!! 63) $ do 
  ((ix, x) : xs) <- tails hashes
  ts <- take 1 $ do
    s <- sliding 3 x
    guard $ BS.all (== BS.head s) s
    pure $ BS.head s
  guard $ any ((BS.replicate 5 ts `BS.isInfixOf`) . snd) $ take 1000 xs
  pure (ix, x)
    where
      hashes :: [(Int, ByteString)]
      hashes = zip [0 ..] $ map (strat . (salt <>) . pack . show) [0 ..]
      sliding n bs = takeWhile ((== n) . BS.length) . map (BS.take 3) $ BS.tails bs

part1 :: ByteString -> Int
part1 = run genHash

part2 :: ByteString -> Int
part2 = run (genHash . (!! 2015) . iterate MD5.hash)

main :: IO ()
main = do
  print $ part1 "ahsbgdzn"
  print $ part2 "ahsbgdzn"

-- 23890
-- 22696 (92 s)
