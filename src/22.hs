{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where
import BasePrelude hiding ((&), cast)
import Control.Lens

---------------
--- some models

data Spell =
  Msl | Drn | Shd | Psn | Rch
  deriving (Show, Eq, Enum, Bounded)

-- | player health
newtype HP = HP Int deriving (Num, Show, Eq, Ord)
-- | boss health
newtype BP = BP Int deriving (Num, Show, Eq, Ord)
-- | player armor
newtype Armor = Armor Int deriving (Num, Show, Eq, Ord)
-- | player mana
newtype Mana = Mana Int deriving (Num, Show, Eq, Ord)

data Game =
  Game { _hp :: HP
       , _bp :: BP
       , _armor ::  Armor
       , _mana :: Mana
       , _effs :: [(Spell, Int)]
       , _spent :: Mana
       , _damage :: BP
       } deriving Show

makeLenses ''Game

cast :: Spell -> Game -> Game
cast Msl = (bp -~ 4) . (mana -~ 53) . (spent +~ 53)
cast Drn = (bp -~ 2) . (hp +~ 2) . (mana -~ 73) . (spent +~ 73)
cast Shd = (effs <>~ [(Shd, 6)]) . (mana -~ 113) . (spent +~ 113)
cast Psn = (effs <>~ [(Psn, 6)]) . (mana -~ 173) . (spent +~ 173)
cast Rch = (effs <>~ [(Rch, 5)]) . (mana -~ 229) . (spent +~ 229)

-- | armor with shield
armor' :: Getter Game Armor
armor' =
  to (\game -> game ^. armor + (if active game Shd then 7 else 0))

-- | returns True if the spell's effects are active (inside the effs
-- set)
active :: Game -> Spell -> Bool
active game spell =
  spell `elem` game ^.. effs . traverse . _1

runEffs :: Game -> Game
runEffs =
  gc . erode . recharge . poison
  where
    poison game = game & bp -~ if active game Psn then 3 else 0
    recharge game = game & mana +~ if active game Rch then 101 else 0
    erode game = game & effs . traverse . _2 -~ 1
    gc game = game & effs %~ filter ((/= 0) . snd)

------------------
-- the game itself

-- | dead players go to the Left and living players go to the Right
-- | by throwing games into the Left bin, we short-circuit all the >=> chains below
-- | (faster solution, easier-to-read output)
hpGuard :: Game -> Either Game Game
hpGuard game =
  if game ^. hp > 0 then Right game else Left game

-- | dead bosses go to the Right and living bosses go the Left
bpGuard :: Game -> Either Game Game
bpGuard game =
  if game ^. bp > 0 then Right game else Left game

player :: Spell -> Game -> Either Game Game
player spell =
  pure . harden
  >=> hpGuard
  >=> pure . runEffs
  >=> pure . cast spell
  >=> manaCheck
  where
    manaCheck game =
      if game ^. mana > 0 then Right game else Left game
    -- set to `id` on easy mode
    harden game =
      game & hp -~ 1

boss :: Game -> Either Game Game
boss =
  pure . runEffs
  >=> hpGuard
  >=> bpGuard
  >=> pure . attack
  where
   attack game =
     game & hp -~ damage' game
   -- player damage = max(1, boss damage - player armor with shields)
   damage' game =
     max 1 (HP (game ^. damage . coerced) - HP (game ^. armor' . coerced))

space :: [[Spell]]
space =
  concat [comb i allSpells | i <- [1 .. n]]
  where
    allSpells :: [Spell]
    allSpells = [minBound..]

-- | for each spell, run player actions then boss actions
play :: Game -> [Spell] -> Either Game Game
play =
  foldlM (\game spell -> (player spell >=> boss) game)

winning :: Game -> [Game]
winning game0 =
  [g | Left g <- play game0 <$> space -- give me all games that end after N turns
     , g ^. bp <= 0 -- with a dead boss
     , g ^. hp > 0 -- and a living player
     , g ^. mana >= 0 -- and nonnegative mana
     ]

main :: IO ()
main = do
  print "start with a low n"
  print "pick an initial game"
  print "run winning on an initial game"
  print "if empty, increment and repeat"

------------
-- utilities

-- | maximum number of steps to search for
-- | the program is O(2^n)
n :: Int
n = 16

-- | all combinations (without replacement) of a given set
comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb i set_ = do
  x <- set_
  (x:) <$> comb (i - 1) set_

--------------------------------
-- initial states (for GHCi use)

start, start', start'' :: Game
-- | the test one
start = Game 50 51 0 500 [] 0 9
-- | easy challenge
start' = Game 10 14 0 250 [] 0 8
-- | hard challenge
start'' = Game 10 13 0 250 [] 0 8
