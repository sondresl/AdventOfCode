{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Day16 where

import Lib (hexToBin)
import Control.Lens (sumOf, makePrisms, Plated, _1, cosmos)
import Control.Monad (replicateM, guard)
import qualified Text.Parsec as P
import Data.Digits (unDigits)
import Data.Tuple.Extra (first, second)
import Data.Semigroup (Sum(..))
import Data.Data (Data)
import Data.Data.Lens (biplate)
import Data.Foldable (fold)
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)

type Parser = P.Parsec [Int] ()

data Packet = Packet Int Int (Either Int [Packet])
  deriving (Show, Data, Eq, Ord)

makePrisms ''Packet
makeBaseFunctor ''Packet
deriving instance Plated Packet

packet :: Parser (Packet, Sum Int)
packet = do
  version <- ints 3
  typeId <- ints 3
  (child, consumed) <- case typeId of
                4 -> first Left <$> literal
                _ -> first Right <$> operator
  pure (Packet version typeId child, consumed + 6)
    where
      ints n = unDigits 2 <$> replicateM n P.anyToken

      operator = P.anyToken >>= \case
          0 -> second (16 <>) <$> (untilN . Sum =<< ints 15)
          1 -> second ((12 <>) . mconcat) . unzip <$> ((`replicateM` packet) =<< ints 11)

      literal = first (unDigits 2) <$> literal'
        where 
          literal' = do 
            (c:bits) <- replicateM 5 P.anyToken
            mappend (bits, Sum 5) <$> if c == 1 then literal' else pure mempty

      untilN n = do
        (ys, m) <- packet
        mappend ([ys], m) <$> if m == n then pure mempty else untilN (n - m)

part2 :: PacketF Int -> Int
part2 (PacketF _ _   (Left i)) = i
part2 (PacketF _ tId (Right subs)) = f subs
  where f = case tId of
              0 -> sum;     1 -> product
              2 -> minimum; 3 -> maximum
              5 -> \[x, y] -> if x >  y then 1 else 0
              6 -> \[x, y] -> if x <  y then 1 else 0
              7 -> \[x, y] -> if x == y then 1 else 0

main :: IO ()
main = do
  let mkTree = either (error . show) fst . P.parse packet ""
  tree <- mkTree . concatMap hexToBin . init <$> readFile "../data/day16.in"
  print $ sumOf (cosmos . _Packet . _1) tree
  print $ cata part2 tree
  
-- 981
-- 299227024091
