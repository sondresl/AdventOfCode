module Day09 where

import Data.Foldable (toList)
import Data.List.Extra (foldl', intersperse)
import Data.Sequence (Seq((:<|), (:|>), Empty))
import qualified Data.Sequence as Seq

type Id = Int
type Size = Int

-- Empty space and Files
data Disk = E Size | F Id Size
  deriving (Show, Eq)

enoughSpace :: Size -> Disk -> Bool
enoughSpace size = \case
  E s   -> s >= size
  F _ _ -> False

move :: Seq Disk -> Seq Disk
move = \case
  Empty -> Empty
  F i s  :<| rest               -> F i s :<| move rest
  E size :<| (rest :|> (E _))   -> move (E size :<| rest)
  E size :<| (rest :|> (F i s))
    | size > s  -> F i s    :<| move (E (size - s) :<| rest)
    | size < s  -> F i size :<| move (rest :|> F i (s - size))
    | size == s -> F i s    :<| move rest

moveWhole :: Seq Disk -> Seq Disk
moveWhole = \case
  Empty              -> Empty
  F i s :<| rest     -> F i s :<| moveWhole rest
  front :|> E i      -> moveWhole front :|> E i
  front :|> F i size -> case Seq.findIndexL (enoughSpace size) front of
    Nothing -> moveWhole front  :|> F i size
    Just ix -> moveWhole front' :|> E size
      where
        (E s) = Seq.index front ix
        front' = if s == size
                   then Seq.update ix (F i size) front
                   else Seq.insertAt ix (F i size)
                      $ Seq.update ix (E $ s - size) front

checksum :: Seq Disk -> Int
checksum = snd . foldl' f (0, 0) . toList
  where
    f (i, total) = \case
       E size       -> (i + size, total)
       F ident size -> (i + size, total + total')
         where total' = sum $ map (* ident) $ take size [i..]

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day09.in"
  print $ checksum $ move input
  print $ checksum $ moveWhole input

parseInput :: String -> Seq Disk
parseInput = Seq.fromList
           . zipWith ($) (intersperse E $ map F [0..])
           . map (read . pure)

-- 6340197768906
-- 6363913128533
