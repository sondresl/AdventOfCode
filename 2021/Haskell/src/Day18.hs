{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Day18 where

import Lib (tuple, (.:))
import Data.Data (Data)
import Control.Lens (makePrisms, Plated)
import Control.Monad (replicateM)
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Text.ParserCombinators.Parsec (many1, digit, parse, char, (<|>))

data BinTree
  = Leaf Int
  | Node BinTree BinTree
  deriving (Show, Eq, Ord, Data)

makePrisms ''BinTree
makeBaseFunctor ''BinTree
deriving instance Plated BinTree

data State = Clean BinTree | Exploded BinTree | Propagating BinTree (Either Int Int) | Boom Int Int 
  deriving Show

propagate :: Either Int Int -> BinTree -> BinTree
propagate (Left n) (Leaf v) = Leaf $ v + n
propagate (Left n) (Node (Leaf v) r) = Node (Leaf (v + n)) r
propagate (Left n) (Node l r) = Node (propagate (Left n) l) r
propagate (Right n) (Leaf v) = Leaf $ v + n
propagate (Right n) (Node l (Leaf v)) = Node l (Leaf (v + n))
propagate (Right n) (Node l r) = Node l (propagate (Right n) r)

explode :: BinTree -> BinTree
explode tree = case go 0 tree of
                 Propagating tree _ -> explode tree
                 Exploded tree -> explode tree
                 Clean tree -> split tree
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

split :: BinTree -> BinTree
split tree = case go tree of
               Left tree -> explode tree
               Right tree -> tree
  where 
    go (Leaf v)
      | v >= 10 = Left $ Node (Leaf $ v `div` 2) (Leaf $ ceiling $ fromIntegral v / 2)
      | otherwise = Right (Leaf v)
    go (Node l r) = case go l of
                      Left l' -> Left $ Node l' r
                      Right l' -> case go r of
                                   Left r' -> Left $ Node l' r'
                                   Right r' -> Right $ Node l' r'

magnitude :: BinTree -> Int
magnitude = cata $ \case
    (LeafF i) -> i
    (NodeF x y) -> 3 * x + 2 * y

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day18.in"
  print . magnitude . foldl1 (explode .: Node) $ input
  print . maximum $ do
    (x, y) <- tuple <$> replicateM 2 input
    map (magnitude . explode) [Node x y, Node y x]

parseInput :: String -> [BinTree]
parseInput = either (error . show) id . traverse (parse p "") . lines
  where
    p = single <|> list
    num = read @Int <$> many1 digit
    single = Leaf <$> num
    list = Node <$> (char '[' *> p <* char ',')
                <*> (p <* char ']')
