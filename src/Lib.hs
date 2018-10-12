{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Protolude

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import qualified Control.Monad.State.Strict as SS
import Data.Bits
import Data.Char (intToDigit)
import Data.List (insertBy)
import qualified Data.Text as T
import qualified Data.Text.Format as F
import Data.Word
import Numeric (showIntAtBase, showHex)
import System.Random.MWC (GenIO, createSystemRandom)
import System.Random.MWC.Distributions (uniformShuffle, uniformShuffleM)
import qualified Text.Show as Show

import Poker.Cards

type StateIO a = SS.StateT a IO

newtype BinWord16 = BW16 Word16
  deriving (Eq, Ord, Num, Bits, FiniteBits)

instance Show BinWord16 where
  show (BW16 w) = "0b" ++ showIntAtBase 2 intToDigit w ""

newtype HexWord32 = HW32 Word32
  deriving (Eq, Ord, Num, Bits, FiniteBits)

instance Show HexWord32 where
  show (HW32 w) = toS $ "0x" <> T.justifyRight 8 '0' (toS $ showHex w "")

newtype HexWord64 = HW64 Word64
  deriving (Eq, Ord, Num, Bits, FiniteBits)

instance Show HexWord64 where
  show (HW64 w) = toS $ "0x" <> T.justifyRight 16 '0' (toS $ showHex w "")

insertOn :: (Ord b) => (a -> b) -> a -> [a] -> [a]
insertOn f = insertBy cmp
  where cmp a b = compare (f a) (f b)

newtype SuitCount = SuitCount HexWord32
  deriving (Show, Eq, Ord, Num, Bits)

newtype RankCount = RankCount HexWord64
  deriving (Show, Eq, Ord, Num, Bits)

newtype RankSet = RankSet BinWord16
  deriving (Show, Eq, Ord, Num, Bits, FiniteBits)

newtype AceRankSet = AceRankSet BinWord16
  deriving (Show, Eq, Ord, Num, Bits, FiniteBits)

data Dup = Dup RankCount Rank
  deriving (Show, Eq, Ord)

data Score
  = HighCard RankSet
  | Pair Rank
         RankSet
  | TwoPair Rank
            Rank
            RankSet
  | Trip Rank
         RankSet
  | Straight Int
  | Flush [Rank]
  | FullHouse Rank
              Rank
  | Quad Rank
         RankSet
  | StraightFlush Int
  deriving (Show, Eq, Ord)

toAce :: RankSet -> AceRankSet
toAce (RankSet xs) = AceRankSet (xs .|. (shiftR xs 13))

suitMask :: SuitCount
suitMask = 0xFF

rankMask :: RankCount
rankMask = RankCount 0xF

straightMask :: AceRankSet
straightMask = 0x1F

score :: Deck -> Score
score h =
  let ss = U.foldl' incSuit (SuitCount 0) h
      rs = U.foldl' incRank (RankCount 0) h
      xs' = U.foldl' setRank (RankSet 0) h
      xs = toAce xs'
      st = findStraight xs
      fl = findFlush ss
      pc = popCount xs'
      ds = countDups rs
   in case (st, fl, pc, ds) of
        (Just i, Just _, _, _) -> StraightFlush i

        (_, Just s, 7, _) ->
          Flush (take 5 . sortOn Down . fmap fst $ U.toList h)
        (Just i, _, 7, _) -> Straight i
        (_, _, 7, _) -> HighCard (trim 2 xs')

        (_, _, _, (Dup 4 n:_)) ->
          Quad n (trim (pc - 2) (xs' `clearBit` (fromEnum n)))
        (_, _, _, (Dup 3 n:Dup 3 m:Dup 2 o:_))
          | o > m -> FullHouse n o
        (_, _, _, (Dup 3 n:Dup 3 m:_)) -> FullHouse n m
        (_, _, _, (Dup 3 n:Dup 2 m:_)) -> FullHouse n m
        (_, Just s, _, _) ->
          Flush (take 5 . sortOn Down . fmap fst $ U.toList h)
        (Just i, _, _, _) -> Straight i
        (_, _, _, (Dup 3 n:_)) -> Trip n (trim 2 (clearBit xs' (fromEnum n)))
        (_, _, _, (Dup 2 n:Dup 2 m:_)) ->
          TwoPair n m
            (trim (pc - 3) (xs' `clearBit` (fromEnum n)
                                `clearBit` (fromEnum m)))
        (_, _, _, (Dup 2 n:_)) -> Pair n (trim 2 (clearBit xs' (fromEnum n)))
  where
    incSuit :: SuitCount -> Card -> SuitCount
    incSuit a (_, Suit s) = a + (1 `shiftL` (s * 8))

    incRank :: RankCount -> Card -> RankCount
    incRank a (Rank r, _) = a + (1 `shiftL` (r * 4))

    setRank :: RankSet -> Card -> RankSet
    setRank a (Rank r, _) = a .|. (1 `shiftL` r)

    findStraight :: AceRankSet -> Maybe Int
    findStraight xs =
      getFirst $
      foldr (goStraight xs) (First Nothing) [countTrailingZeros xs .. 9]

    goStraight :: AceRankSet -> Int -> First Int -> First Int
    goStraight xs i m = isStraight xs i <> m

    isStraight :: AceRankSet -> Int -> First Int
    isStraight xs i =
      First $
      let m = straightMask `shiftL` i
       in if (m .&. xs) == m
            then Just (i + 1)
            else Nothing

    findFlush :: SuitCount -> Maybe Suit
    findFlush rs = getFirst $ foldr (goFlush rs) (First Nothing) [0 .. 3]

    goFlush :: SuitCount -> Int -> First Suit -> First Suit
    goFlush ss i m = isFlush ss i <> m

    isFlush :: SuitCount -> Int -> First Suit
    isFlush ss i =
      First $
      if (ss `shiftR` (i * 8)) .&. suitMask >= 5
        then Just (toEnum i)
        else Nothing

    countDups :: RankCount -> [Dup]
    countDups rs =
      foldl' (flip $ insertOn Down) [] $ fmap (goDup rs) [deuce .. ace]

    goDup :: RankCount -> Rank -> Dup
    goDup rs r@(Rank n) =
      Dup ((rs `shiftR` (4 * n)) .&. rankMask) r

    trim :: Int -> RankSet -> RankSet
    trim 0 rs = rs
    trim n rs =
      let rs' = clearBit rs (countTrailingZeros rs)
       in trim (n - 1) rs'

newDeck :: Deck
newDeck = U.fromList $ do
  s <- [minBound .. maxBound]
  r <- [minBound .. maxBound]
  pure (r, s)

shuffle :: GenIO -> StateIO Deck ()
shuffle rng = do
  d <- get
  d' <- uniformShuffle d rng
  put d'

draw :: Card -> StateIO Deck ()
draw c =
  modify $ \d ->
    case U.elemIndex c d of
      Nothing -> d
      Just i -> U.tail $ U.modify (\m -> MU.swap m 0 i) d

deal :: Int -> StateIO Deck Deck
deal n = do
  (f, b) <- U.splitAt n <$> get
  put b
  pure f

playHand :: GenIO -> Deck -> Int -> StateIO Deck Double
playHand rng h n = do
  shuffle rng
  U.forM_ h draw
  hs <- replicateM n (deal 2)
  cs <- deal 5
  let s = score (h <> cs)
      ss = fmap (score . (<> cs)) hs
      e = share s ss 1
  if e == 0
  then pure 0
  else pure ((1 + fromIntegral n) / e)
  where
   share s [] x = x
   share s (s' : ss) x = case compare s s' of
                           GT -> share s ss x
                           EQ -> share s ss (x + 1)
                           LT -> 0

playRounds :: GenIO -> Deck -> Int -> Int -> IO Double
playRounds rng h n r = do
  rs <- replicateM r (SS.evalStateT (playHand rng h n) newDeck)
  pure (sum rs / fromIntegral (r * (n + 1)))

hands :: [Deck]
hands =
  fmap
    U.fromList
    [ [(ace, spade), (ace, heart)]
    , [(king, spade), (king, heart)]
    , [(queen, spade), (queen, heart)]
    , [(jack, spade), (jack, heart)]
    , [(ten, spade), (ten, heart)]
    , [(ace, spade), (king, spade)]
    , [(ace, spade), (king, heart)]
    , [(deuce, spade), (deuce, heart)]
    , [(deuce, spade), (seven, heart)]
    ]

someFunc :: IO ()
someFunc = do
  rng <- createSystemRandom

  forM_ hands $ \h -> do
    rs <- forM [1 .. 9] $ \n -> do
      playRounds rng h 1 10000
    let fs = fmap (F.build " {}%" . F.Only . F.fixed 1 . (* 100)) rs
    F.print "{}{}\n" (show h :: Text, mconcat fs)
  pure ()
