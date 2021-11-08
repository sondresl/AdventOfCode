module Day21 where

import Lib
import Control.Lens
import Control.Monad (guard)

data Unit = Unit
  { _name :: String
  , _hp :: Int
  , _dmg :: Int
  , _arm :: Int
  }
  deriving (Show, Eq, Ord)
makeLenses ''Unit

boss :: Unit
boss = Unit "Boss" 103 9 2

play :: Unit -> Unit -> String
play u@(Unit _ hp dmg _) (Unit s hp' dmg' arm')
  | hp <= 0 = s
  | otherwise = play (Unit s (hp' - max 1 (dmg - arm')) dmg' arm') u

mkUnit :: Weapon -> Armour -> Ring -> Ring -> (Int, Unit)
mkUnit (Weapon _ c d a)
       (Armour _ c' d' a')
       (Ring _ rc rdmg rarm)
       (Ring _ rc' rdmg' rarm') =
  ( c + c' + rc + rc'
  , Unit "Player" 100
                  (d + d' + rdmg + rdmg')
                  (a + a' + rarm + rarm')
  )

run :: String -> ([Int] -> Int) -> Int
run winner f = f $ do
  wp <- weapons
  ar <- armour
  (r1, rest) <- select rings
  r2 <- rest
  let (cost, player) = mkUnit wp ar r1 r2
  guard $ play player boss == winner
  pure cost

part1 :: Int
part1 = run "Player" minimum

part2 :: Int
part2 = run "Boss" maximum

main :: IO ()
main = do
  print part1
  print part2

-- 121
-- 201

data Weapon = Weapon String Int Int Int
  deriving (Show, Eq, Ord)
data Armour = Armour String Int Int Int
  deriving (Show, Eq, Ord)
data Ring = Ring String Int Int Int
  deriving (Show, Eq, Ord)


-- Weapons:    Cost  Damage  Armor
weapons :: [Weapon]
weapons = [ Weapon "Dagger"        8     4       0
          , Weapon "Shortsword"   10     5       0
          , Weapon "Warhammer"    25     6       0
          , Weapon "Longsword"    40     7       0
          , Weapon "Greataxe"     74     8       0
          ]

-- Armor:      Cost  Damage  Armor
armour :: [Armour]
armour = [ Armour "None"          0     0       0
         , Armour "Leather"      13     0       1
         , Armour "Chainmail"    31     0       2
         , Armour "Splintmail"   53     0       3
         , Armour "Bandedmail"   75     0       4
         , Armour "Platemail"   102     0       5
         ]

-- Rings:      Cost  Damage  Armor
rings :: [Ring]
rings = [ Ring "Damage 0"     0     0       0 -- No ring
        , Ring "Damage 0"     0     0       0 -- No ring
        , Ring "Damage 1"    25     1       0
        , Ring "Damage 2"    50     2       0
        , Ring "Damage 3"   100     3       0
        , Ring "Defense 1"   20     0       1
        , Ring "Defense 2"   40     0       2
        , Ring "Defense 3"   80     0       3
        ]
