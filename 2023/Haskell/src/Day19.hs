{-# LANGUAGE TemplateHaskell #-}
module Day19 where

import Lib ((.:), tuple, allNums)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
    (parse, many1, letter, between, char, try, (<|>), sepBy1, oneOf, digit, Parser)
import qualified Data.Interval as I
import Data.Char (toUpper)
import Linear (V4(..))
import Control.Lens.TH (makeLenses)
import Control.Lens (set, (&), (.~), Field1(_1), Field2(_2), Field3(_3), Field4(_4))

type Rating = V4 Int
type Work = V4 (I.Interval Int)
data WF = WF Work Work String
  deriving Show

defaultInterval :: Work
defaultInterval = pure $ 1 I.<=..<= 4000

reduceWF :: Map String [WF] -> [Work]
reduceWF input = go defaultInterval (input Map.! "in")
  where
    go _   []                      = []
    go rating (WF pass fail "A":wfs)  = [combine rating pass]                       <> go (combine rating fail) wfs
    go rating (WF pass fail "R":wfs)  = []                                          <> go (combine rating fail) wfs
    go rating (WF pass fail dest:wfs) = go (combine rating pass) (input Map.! dest) <> go (combine rating fail) wfs
    combine = liftA2 I.intersection

accept :: [Work] -> Rating -> Bool
accept works rat = any (and . liftA2 I.member rat) works

main :: IO ()
main = do
  (ratings, flows) <- parseInput <$> readFile "../data/day19.in"
  let reduced = reduceWF flows
  print . sum . map sum $ filter (accept reduced) ratings
  print . sum $ map (product . fmap (succ . I.width)) reduced

parseInput :: String -> ([Rating], Map String [WF])
parseInput input = (ratings', Map.fromList $ map parseFlow flows)
  where
    (flows, ratings) = tuple . map lines $ splitOn "\n\n" input
    ratings' = map ((\[x,m,a,s] -> V4 x m a s) . allNums) ratings
    parseFlow = either (error . show) id . parse p ""
    p = (,) <$> many1 letter
            <*> between (char '{') (char '}') ((try parseWork <|> parseLetters) `sepBy1` char ',')
    parseWork = do
      rat <- map toUpper <$> many1 letter
      comp <- oneOf "<>"
      n <- fromIntegral . read @Int <$> (many1 digit <* char ':')
      dest <- many1 letter
      let f :: I.Interval Int -> Work
          f i = set (case rat of "X" -> _1; "M" -> _2; "A" -> _3; "S" -> _4) i defaultInterval
      pure $ if comp == '>' 
                 then WF (f $ (n + 1) I.<=..<= 4000)    (f $ 1 I.<=..<= n   ) dest
                 else WF (f $ 1       I.<=..<= (n - 1)) (f $ n I.<=..<= 4000) dest
    parseLetters = WF defaultInterval defaultInterval <$> many1 letter

-- 332145
-- 136661579897555
