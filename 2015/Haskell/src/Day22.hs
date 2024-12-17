module Day22 where

import Control.Lens
import Control.Monad

-- import Debug.Trace

data Player = Player
  { _hp :: Int
  , _armor :: Int
  , _mana :: Int
  , _shieldTime :: Int
  , _rechargeTime :: Int
  }
  deriving (Show, Eq, Ord)
makeLenses ''Player

-- Puzzle input
boss :: (Int, Int, Int)
boss = (55, 8, 0)

player :: Player
player = Player 50 0 500 0 0

applyDefensive :: Player -> Player
applyDefensive player =
  player & armor +~ case view shieldTime player of
                      1 -> (-7)
                      7 -> 7
                      _ -> 0
         & mana +~ case view rechargeTime player of
                     0 -> 0
                     _ -> 101
         & rechargeTime .~ max 0 (view rechargeTime player - 1)
         & shieldTime .~ max 0 (view shieldTime player - 1)

applyOffensive :: (Int, Int, Int) -> (Int, Int, Int)
applyOffensive boss =
  boss & _1 +~ case view _3 boss of
                 0 -> 0
                 _ -> (-3)
       & _3 .~ max 0 (view _3 boss - 1)

-- Might need to refactor into BFS rather than DFS.
-- Or, find alternative solution.
play :: (Player -> Player) -> Player -> (Int, Int, Int) -> Int
play f pl bo = minimum $ playerTurn pl bo 0
  where
    playerTurn :: Player -> (Int, Int, Int) -> Int -> [Int]
    playerTurn (f -> player) boss total = do
      guard $ view hp player > 0
      let player' = applyDefensive player
          boss' = applyOffensive boss
          remainingMana = view mana player'
      (name, cost) <- filter ((<= remainingMana) . view _2) spells
      guard $ if | name == "Poison" && view _3 boss > 0 -> False
                 | name == "Shield" && view shieldTime player' > 0 -> False
                 | name == "Recharge" && view rechargeTime player > 0 -> False
                 | otherwise -> True
      let total' = total + cost
          (player'', boss'') = case name of
                                 "Magic Missile" -> (player'                    , boss' & _1 -~ 4)
                                 "Drain"         -> (player' & hp +~ 2          , boss' & _1 -~ 2)
                                 "Shield"        -> (player' & shieldTime .~ 7  , boss')
                                 "Poison"        -> (player'                    , boss' & _3 .~ 6)
                                 "Recharge"      -> (player' & rechargeTime .~ 5, boss')
          player''' = player'' & mana -~ cost
      bossTurn player''' boss'' total'
    bossTurn :: Player -> (Int, Int, Int) -> Int -> [Int]
    bossTurn player boss total = do
      if view _1 boss <= 0
         then pure total
         else let player' = applyDefensive player
                  boss' = applyOffensive boss
                  arm = view armor player'
                  dmg = view _2 boss'
                  player'' = player & hp -~ max 1 (dmg - arm)
               in playerTurn player'' boss' total


part1 :: Int
part1 = play id player boss

part2 :: Int
part2 = play (hp +~ (-1)) player boss

main :: IO ()
main = do
  print part1
  print part2

spells = [ ("Magic Missile", 53)
         , ("Drain", 73)
         , ("Shield", 113)
         , ("Poison", 173)
         , ("Recharge", 229)
         ]
