{-# LANGUAGE RecordWildCards #-}
module Cards (sortBy, on, module Cards) where

import Data.List (sortBy)
import Data.Function (on)

data Suit = Heart | Clubs | Diamond | Spades
          deriving (Eq, Ord, Enum)
instance Show Suit where
  show Heart   = "♥"
  show Clubs   = "♣"
  show Diamond = "♦"
  show Spades  = "♠"

data Rank = Ace   | King  | Queen | Jack | Ten | Nine
          | Eight | Seven | Six   | Five | Four| Three
          | Two deriving (Eq, Ord, Enum)

instance Show Rank where
  show Ace       = "A"
  show King      = "K"
  show Queen     = "Q"
  show Jack      = "J"
  show Ten       = "10"
  show Nine      = "9"
  show Eight     = "8"
  show Seven     = "7"
  show Six       = "6"
  show Five      = "5"
  show Four      = "4"
  show Three     = "3"
  show Two       = "2"

data Card = Card {suit :: Suit, rank :: Rank}
          deriving (Eq)

instance Show Card where
  show Card{..} = show suit ++"/"++show rank

allCards :: [Card]
allCards = [Card s r | s <- [Heart ..], r <- [Ace ..]]
