module Day09 where

import Data.Maybe
import Data.Foldable (toList)
import Data.List.Extra (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq((:<|), (:|>), Empty), (><))
import qualified Data.Sequence as Seq

type Id = Int
type Size = Int

data Disk = E Size | F Id Int
  deriving (Show, Eq)

enoughSpace :: Int -> Disk -> Bool
enoughSpace size (E s) = s >= size
enoughSpace size _ = False

move :: Seq Disk -> Seq Disk
move Empty = Empty
move (f@(F i size) :<| rest)               = f :<| move rest
move (f@(E size)   :<| (rest :|> (E _)))   = move (f :<| rest)
move (f@(E size)   :<| (rest :|> (F i s)))
  | size > s  = F i s    :<| move (E (size - s) :<| rest)
  | size < s  = F i size :<| move (rest :|> F i (s - size))
  | size == s = F i s    :<| move rest

moveWhole :: Seq Disk -> Seq Disk
moveWhole Empty = Empty
moveWhole (f@(F _ _) :<| rest) = f :<| (moveWhole rest)
moveWhole (front :|> E i) = (moveWhole front) :|> E i
moveWhole (front :|> F i size) = 
  case Seq.findIndexL (enoughSpace size) front of
    Nothing -> (moveWhole front) :|> F i size
    Just ix -> let (E s) = Seq.index front ix
                   front' = if s == size 
                                then Seq.update ix (F i size) front
                                else Seq.insertAt ix (F i size) 
                                   $ Seq.update ix (E $ s - size) front
                in (moveWhole front') :|> (E size)

checksum :: Seq Disk -> Int
checksum = snd . foldl' f (0, 0) . toList
  where
    f (i, total) (E s) = (i + s, total)
    f (i, total) (F ident size) = 
      let total' = sum $ map (* ident) $ take size [i..]
       in (i + size, total + total')

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day09.in"
  print $ checksum $ move input
  print $ checksum $ moveWhole input

parseInput :: String -> Seq Disk
parseInput = Seq.fromList . go files . map (read . pure) . init
  where
    files = map F [0..]
    go (f:fs) [] = []
    go (f:fs) [x] = [f x]
    go (f:fs) (x:y:xs) = f x : E y : go fs xs

-- 6340197768906
-- 6363913128533
