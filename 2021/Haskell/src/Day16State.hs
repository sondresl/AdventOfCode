{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Day16State where

import Lib (hexToBin)
import Control.Lens (sumOf, makePrisms, Plated, _1, cosmos, (<&>))
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

import Debug.Trace

type Parser = P.Parsec [Int] Int

data Packet = Packet Int Int (Either Int [Packet])
  deriving (Show, Data, Eq, Ord)

makePrisms ''Packet
makeBaseFunctor ''Packet
deriving instance Plated Packet

packet :: Parser Packet
packet = do
  (version, typeId) <- (,) <$> ints 3 <*> ints 3
  child <- case typeId of 
             4 -> Left <$> literal
             _ -> Right <$> operator
  pure $ Packet version typeId child
    where
      end n = guard . (== n) =<< P.getState
      ints n = unDigits 2 <$> replicateM n anyToken
      anyToken = P.modifyState (+1) >> P.anyToken

      operator = anyToken >>= \case
          0 -> ((+) <$> ints 15 <*> P.getState) >>= P.manyTill packet . end
          1 -> ints 11 >>= (`replicateM` packet)

      literal = unDigits 2 <$> literal'
        where 
          literal' = do 
            (c:bits) <- replicateM 5 anyToken
            (bits <>) <$> if c == 1 then literal' else pure mempty

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
  let mkTree = either (error . show) id . P.runParser packet 0 ""
  tree <- mkTree . concatMap hexToBin . init <$> readFile "../data/day16.in"
  print $ sumOf (cosmos . _Packet . _1) tree
  print $ cata part2 tree
  
-- 981
-- 299227024091
