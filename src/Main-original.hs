{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.List
import qualified Data.Map as Map
import           Prelude

data Status = On | Off deriving Eq
data Action = TurnOn | TurnOff | Toggle deriving Show
type Coord = (Int, Int)
data Range = Range { from :: Coord, to :: Coord } deriving Show
data Command = Command Action Range deriving Show
type Yard = Map.Map Coord Status

commandParser :: Parser Command
commandParser = do
  action <- actionParser
  string " "
  range <- rangeParser
  return $ Command action range

  where
    actionParser :: Parser Action
    actionParser = (string "turn on" >> return TurnOn)
               <|> (string "turn off" >> return TurnOff)
               <|> (string "toggle" >> return Toggle)

    rangeParser :: Parser Range
    rangeParser = do
      c1 <- coordParser
      string " through "
      c2 <- coordParser
      return $ Range c1 c2

    coordParser :: Parser Coord
    coordParser = do
      d1 <- decimal
      string ","
      d2 <- decimal
      return (d1, d2)

parseCommand :: String -> Command
parseCommand input = case parsed of
  Left e -> error $ input ++ " gave " ++ e
  Right cs-> cs
  where
    parsed = parseOnly commandParser . BS.pack $ input

expandRange :: Range -> [Coord]
expandRange (Range (x1, y1) (x2, y2)) = do
  x <- [x1..x2]
  y <- [y1..y2]
  [(x, y)]

process :: Yard -> Command -> Yard
process yard (Command action range) = foldl' single yard (expandRange range)
  where
    single yard coord = updateYard action coord yard

updateYard :: Action -> Coord -> Yard -> Yard
updateYard TurnOn c = Map.insert c On
updateYard TurnOff c = Map.insert c Off
updateYard Toggle c = Map.alter toggle c
  where
    toggle Nothing = Just On
    toggle (Just Off) = Just On
    toggle (Just On) = Just Off

countLit :: Yard -> Int
countLit = Map.size . Map.filter (== On)

main :: IO ()
main = do
  commands <- lines <$> getContents
  print . countLit . foldl process Map.empty . fmap parseCommand $ commands
