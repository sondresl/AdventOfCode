{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Day18 where

import Lib (tuple, (.:))
import Data.Data (Data)
import Control.Lens (makePrisms, Plated)
import Control.Monad (replicateM)
import Data.Semigroup
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Text.ParserCombinators.Parsec (many1, digit, parse, char, (<|>))

data BinTree a
  = Leaf a
  | Node (BinTree a) (BinTree a)
  deriving (Show, Eq, Ord, Data, Functor, Foldable)

type Snail = BinTree Int

makePrisms ''BinTree
makeBaseFunctor ''BinTree
deriving instance Plated Snail

data State = Clean Snail | Exploded Snail | Propagating Snail (Either Int Int) | Boom Int Int
  deriving Show

propagate :: Either Int Int -> Snail -> Snail
propagate (either id id -> n) (Leaf v) = Leaf (v + n)
propagate (Left n) (Node l r) = Node (propagate (Left n) l) r
propagate (Right n) (Node l r) = Node l (propagate (Right n) r)

explode :: Snail -> Snail
explode tree = case go 0 tree of
                 Clean tree -> split tree
                 Exploded tree -> explode tree
                 Propagating tree _ -> explode tree
  where
    go 4 (Node (Leaf l) (Leaf r)) = Boom l r
    go n (Leaf v) = Clean (Leaf v)
    go n (Node l r) =
      case go (n + 1) l of 
        Boom lv rv -> Propagating (Node (Leaf 0) (propagate (Left rv) r)) (Left lv)
        Exploded tree' -> Exploded (Node tree' r)
        Propagating tree' (Right val) -> Exploded (Node tree' (propagate (Left val) r))
        Propagating tree' val -> Propagating (Node tree' r) val
        Clean leftTree -> case go (n + 1) r of 
                            Clean tree' -> Clean $ Node leftTree tree'
                            Boom lv rv -> Propagating (Node (propagate (Right lv) leftTree) (Leaf 0)) (Right rv)
                            Propagating tree' (Left val) -> Exploded (Node (propagate (Right val) leftTree) tree')
                            Propagating tree' val -> Propagating (Node leftTree tree') val
                            Exploded tree' -> Exploded (Node leftTree tree')

split :: Snail -> Snail
split = either explode id . go
  where 
    go (Leaf v)
      | v >= 10 = Left $ Node (Leaf $ v `div` 2) (Leaf $ (v + 1) `div` 2)
      | otherwise = Right (Leaf v)
    go (Node l r) = 
      case (go l, gor) of 
        (Left  l', _       ) -> Left  $ Node l' r 
        (Right l', Left r' ) -> Left  $ Node l' r'
        (Right l', Right r') -> Right $ Node l' r'

magnitude :: Snail -> Int
magnitude = cata $ \case
    LeafF i -> i
    NodeF x y -> 3 * x + 2 * y

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day18.in"
  print . magnitude . foldl1 (explode .: Node) $ input
  print . maximum . map (magnitude . explode . uncurry Node . tuple) $ replicateM 2 input

parseInput :: String -> [Snail]
parseInput = either (error . show) id . traverse (parse f "") . lines
  where
    f = (Leaf . read <$> many1 digit) <|>
        (Node <$> (char '[' *> f <* char ',')
              <*> (f <* char ']'))

-- 2501
-- 4935
