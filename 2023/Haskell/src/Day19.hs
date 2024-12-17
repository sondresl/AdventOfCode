module Day19 where

import Lib (tuple, allNums)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
    (parse, many1, letter, between, char, try, (<|>), sepBy1, oneOf, digit, Parser)
import qualified Data.Interval as I
import Data.Char (toUpper)

data Status = Accepted | Rejected
  deriving (Show, Eq, Ord)

data Category = X | M | A | S
  deriving (Read, Show, Eq, Ord)

lookupCat :: Rating a -> Category -> a
lookupCat rating = \case
  X -> x rating
  M -> m rating
  A -> a rating
  S -> s rating

updateRating :: Rating a -> Category -> a -> Rating a
updateRating rating cat val = 
  case cat of
    X -> rating { x = val }
    M -> rating { m = val }
    A -> rating { a = val }
    S -> rating { s = val }

data Rating a = Rating
  { x :: a
  , m :: a
  , a :: a
  , s :: a
  } deriving (Show, Eq, Ord)

defaultInterval :: Rating (I.Interval Int)
defaultInterval = Rating i i i i
  where i = 1 I.<=..<= 4000

data Workflow = Workflow
  { test :: (Category, I.Interval Int)
  , dest :: String
  } deriving Show

type Workflows = Map String [Workflow]

part1 :: Workflows -> [Rating Int] -> Int
part1 flows = sum . map sumRating . filter ((==Accepted) . runFlow flows)
  where sumRating (Rating x m a s) = x + m + a + s

reduceWorkflow :: Map String [Workflow] -> [Rating (I.Interval Int)]
reduceWorkflow flows = go defaultInterval (flows Map.! "in")
  where
    go ivs [Workflow _ "A"] = [ivs]
    go ivs [Workflow _ "R"] = []
    go ivs [Workflow _ pattern] = go ivs (flows Map.! pattern)
    go ivs ((Workflow (cat, iv) dest):next) = passTest <> go failRating next
      where
        oldIv = lookupCat ivs cat
        passTest = case dest of
          "A" -> [successRating] 
          "R" -> []
          _ -> go successRating (flows Map.! dest)
        successRating = updateRating ivs cat (I.intersection iv oldIv)
        failRating = updateRating ivs cat failIv
        failIv = if I.lowerBound iv == I.Finite 1
                    then I.intersection ((I.upperBound iv + 1) I.<=..<= 4000) oldIv
                    else I.intersection (1 I.<=..<= subtract 1 (I.lowerBound iv)) oldIv

runFlow :: Workflows -> Rating Int -> Status
runFlow ws rating = go (ws Map.! "in")
  where
    go ((Workflow (cat, iv) dest):wf)
      | lookupCat rating cat `I.member` iv = next dest
      | otherwise = go wf
      where
        next "A" = Accepted
        next "R" = Rejected
        next xs = go (ws Map.! dest)

countPerms :: Rating (I.Interval Int) -> Int
countPerms (Rating x m a s) = product $ map (succ . I.width) [x,m,a,s]

main :: IO ()
main = do
  input <- parseInput <$> readFile "../data/day19.in"
  let (ratings, flows) = input
      reduced = reduceWorkflow flows
  print $ part1 flows ratings
  print . sum $ map countPerms reduced

parseInput :: String -> ([Rating Int], Map String [Workflow])
parseInput input = (ratings', Map.fromList $ map parseFlow flows)
  where
    (flows, ratings) = tuple . map lines $ splitOn "\n\n" input
    ratings' = map ((\[x,m,a,s] -> Rating x m a s) . allNums) ratings
    parseFlow :: String -> (String, [Workflow])
    parseFlow = either (error . show) id . parse p ""
    p = (,) <$> many1 letter
            <*> between (char '{') (char '}') ((try parseWork <|> parseLetters) `sepBy1` char ',')
    parseWork = do
      rat <- read . map toUpper <$> many1 letter
      comp <- oneOf "<>"
      n <- fromIntegral . read @Int <$> (many1 digit <* char ':')
      dest <- many1 letter
      let fn = if comp == '>' then (n + 1) I.<=..<= 4000 else 1 I.<=..<= (n - 1)
      pure $ Workflow (rat, fn) dest
    -- Just pick a Category and make it unrestricted
    parseLetters = Workflow (X, 1 I.<=..<= 4000) <$> many1 letter

-- 332145
-- 136661579897555
