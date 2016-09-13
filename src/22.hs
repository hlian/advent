{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where
import BasePrelude hiding ((&), cast)
import Control.Lens

data Spell =
  Msl | Drn | Shd | Psn | Rch
  deriving (Show, Eq, Enum, Bounded)

newtype HP = HP Int deriving (Num, Show, Eq, Ord)
newtype BP = BP Int deriving (Num, Show, Eq, Ord)
newtype Armor = Armor Int deriving (Num, Show, Eq, Ord)
newtype Mana = Mana Int deriving (Num, Show, Eq, Ord)

data Game =
  Game { _hp :: HP, _bp :: BP, _armor ::  Armor, _mana :: Mana, _effs :: [(Spell, Int)], _spent :: Mana, _bd :: BP }
  deriving (Show)

makeLenses ''Game

armor' :: Getter Game Armor
armor' =
  to (\game -> game ^. armor + (if active game Shd then 7 else 0))

cast :: Spell -> Game -> Game
cast spell game =
  case spell of
    Msl -> game & bp -~ 4 & mana -~ 53 & spent +~ 53
    Drn -> game & bp -~ 2 & hp +~ 2 & mana -~ 73 & spent +~ 73
    Shd -> game & effs <>~ [(Shd, 6)] & mana -~ 113 & spent +~ 113
    Psn -> game & effs <>~ [(Psn, 6)] & mana -~ 173 & spent +~ 173
    Rch -> game & effs <>~ [(Rch, 5)] & mana -~ 229 & spent +~ 229

active :: Game -> Spell -> Bool
active game spell =
  spell `elem` game ^.. effs . traverse . _1

runEffs =
  gc . erode . recharge . poison
  where
    poison game = game & bp -~ if active game Psn then 3 else 0
    recharge game = game & mana +~ if active game Rch then 101 else 0
    erode game = game & effs . traverse . _2 -~ 1
    gc game = game & effs %~ filter ((/= 0) . snd)

hpCheck game =
  if game ^. hp > 0 then Right game else Left game

player :: Spell -> Game -> Either Game Game
player spell =
  manaCheck <=<
  pure . cast spell <=<
  pure . runEffs <=<
  hpCheck <=<
  pure . oof
  where
    manaCheck game =
      if game ^. mana > 0 then Right game else Left game
    -- set to `id` on easy mode
    oof game =
      game & hp -~ 1

boss :: Game -> Either Game Game
boss =
  pure . runEffs >=>
  hpCheck >=>
  bpCheck >=>
  pure . attack
  where
   attack game =
     game & hp -~ damage' game
    bpCheck game =
      if game ^. bp > 0 then Right game else Left game
    damage' game =
      max 1 (case game ^. bd of BP i -> HP i - case game ^. armor' of Armor i -> HP i)

step :: Spell -> Game -> Either Game Game
step spell = player spell >=> boss

comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb i universe = do
  x <- universe
  (x:) <$> comb (i - 1) universe

space :: [[Spell]]
space = concat [comb i xs | i <- [1..16]]
  where xs = [(minBound :: Spell)..]

play :: Game -> [Spell] -> Either Game Game
play game0 spells = foldlM (\g s -> step s g) game0 spells

valid game0 = [g | Left g <- play game0 <$> space, g ^. bp <= 0, g ^. hp > 0, g ^. mana >= 0]

-- for debugging

start = Game 50 51 0 500 [] 0 9
start' = Game 10 14 0 250 [] 0 8
start'' = Game 10 13 0 250 [] 0 8
