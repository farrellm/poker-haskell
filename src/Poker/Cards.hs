{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}

module Poker.Cards
    ( Rank(..)
    , Suit(..)
    , Card
    , Deck
    , ace
    , king
    , queen
    , jack
    , ten
    , nine
    , eight
    , seven
    , six
    , five
    , four
    , three
    , deuce
    , spade
    , diamond
    , heart
    , club
    ) where

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving


newtype Rank = Rank
  { getRank :: Int
  } deriving (Eq, Ord)

derivingUnbox "Rank"
  [t| Rank -> Int |]
  [| \ (Rank r) -> r |]
  [| \ r -> Rank r |]

newtype Suit = Suit
  { getSuit :: Int
  } deriving (Eq, Ord)

derivingUnbox "Suit"
  [t| Suit -> Int |]
  [| \(Suit s) -> s |]
  [| \s -> Suit s |]

instance Show (Rank) where
  show (Rank 13) = "A"
  show (Rank 12) = "K"
  show (Rank 11) = "Q"
  show (Rank 10) = "J"
  show (Rank 9) = "T"
  show (Rank r) = show (r + 1)

ace :: Rank
ace = Rank 13

king :: Rank
king = Rank 12

queen :: Rank
queen = Rank 11

jack :: Rank
jack = Rank 10

ten :: Rank
ten = Rank 9

nine :: Rank
nine = Rank 8

eight :: Rank
eight = Rank 7

seven :: Rank
seven = Rank 6

six :: Rank
six = Rank 5

five :: Rank
five = Rank 4

four :: Rank
four = Rank 3

three :: Rank
three = Rank 2

deuce :: Rank
deuce = Rank 1

instance Bounded Rank where
  minBound = deuce
  maxBound = ace

instance Enum Rank where
  fromEnum (Rank r) = r
  toEnum i | getRank deuce <= i && i <= getRank ace = Rank i

instance Show Suit where
  show (Suit 0) = "♠"
  show (Suit 1) = "♥"
  show (Suit 2) = "♦"
  show (Suit 3) = "♣"

spade :: Suit
spade = Suit 0

heart :: Suit
heart = Suit 1

diamond :: Suit
diamond = Suit 2

club :: Suit
club = Suit 3

instance Bounded Suit where
  minBound = spade
  maxBound = club

instance Enum Suit where
  fromEnum (Suit s)
    | s == getSuit spade = 0
    | s == getSuit heart = 1
    | s == getSuit diamond = 2
    | s == getSuit club = 3
  toEnum i
    | i == 0 = spade
    | i == 1 = heart
    | i == 2 = diamond
    | i == 3 = club

type Card = (Rank, Suit)
type Deck = U.Vector Card
