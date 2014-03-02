module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Map as Map

data Suit = Clubs | Spades | Hearts | Diamonds deriving (Show, Eq, Ord)
data Rank = Rank Integer deriving (Show, Eq, Ord)

data Card = Card Rank Suit deriving (Show, Eq)
instance Ord Card where
  compare (Card r1 s1) (Card r2 s2) = compare r1 r2

-- should probs make Eq agree with Ord here
data Hand = Hand [Card] deriving (Show, Eq)
instance Ord Hand where
  compare (Hand cs1) (Hand cs2) = mconcat $ zipWith compare
                                  (sortBy (flip compare) cs1)
                                  (sortBy (flip compare) cs2)

data PokerHand = HighCard Hand | Pair Hand | TwoPair Hand | ThreeOfAKind Hand |
                 Straight Hand | Flush Hand | FullHouse Hand |
                 FourOfAKind Hand | StraightFlush Hand | RoyalFlush
                 deriving (Show, Eq)
-- I should probably just derive ord for this class and then have another function that does
-- additional comparison when PokerHands are "equal"
instance Ord PokerHand where
  compare RoyalFlush RoyalFlush = EQ
  compare RoyalFlush _ = GT
  compare _ RoyalFlush = LT
  compare (StraightFlush h1) (StraightFlush h2) = compare h1 h2
  compare (StraightFlush h1) _ = GT
  compare _ (StraightFlush h2) = LT
  compare (FourOfAKind h1) (FourOfAKind h2) = mappend quadComp kickerComp
    where
      quadComp = compare quadRank1 quadRank2
      kickerComp = compare kickerRank1 kickerRank2
      quadRank1 = firstKeyWithVal freqs1 4
      quadRank2 = firstKeyWithVal freqs2 4
      kickerRank1 = firstKeyWithVal freqs1 1
      kickerRank2 = firstKeyWithVal freqs2 1
      freqs1 = rankFreqs h1
      freqs2 = rankFreqs h2
  compare (FourOfAKind h1) _ = GT
  compare _ (FourOfAKind h2) = LT
  compare (FullHouse h1) (FullHouse h2) = mappend tripComp pairComp
    where
      tripComp = compare tripRank1 tripRank2
      pairComp = compare pairRank1 pairRank2
      tripRank1 = firstKeyWithVal freqs1 3
      tripRank2 = firstKeyWithVal freqs2 3
      pairRank1 = firstKeyWithVal freqs1 2
      pairRank2 = firstKeyWithVal freqs2 2
      freqs1 = rankFreqs h1
      freqs2 = rankFreqs h2
  compare (FullHouse h1) _ = GT
  compare _ (FullHouse h2) = LT
  compare (Flush h1) (Flush h2) = compare h1 h2
  compare (Flush h1) _ = GT
  compare _ (Flush h2) = LT
  compare (Straight h1) (Straight h2) = compare h1 h2
  compare (Straight h1) _ = GT
  compare _ (Straight h2) = LT
  compare (ThreeOfAKind h1) (ThreeOfAKind h2) = mappend tripComp kickersComp
    where
      tripComp = compare tripRank1 tripRank2
      kickersComp = mconcat $ zipWith compare
                    (reverse $ sort kickers1) (reverse $ sort kickers2)
      tripRank1 = firstKeyWithVal freqs1 3
      tripRank2 = firstKeyWithVal freqs2 3
      kickers1 = map fst $ Map.toList $ Map.filter (/= 3) freqs1
      kickers2 = map fst $ Map.toList $ Map.filter (/= 3) freqs2
      freqs1 = rankFreqs h1
      freqs2 = rankFreqs h2
  compare (ThreeOfAKind h1) _ = GT
  compare _ (ThreeOfAKind h2) = LT
  compare (TwoPair h1) (TwoPair h2) = mappend pairsComp kickersComp
    where
      pairsComp = mconcat $ zipWith compare
                  (reverse $ sort pairs1) (reverse $ sort pairs2)
      kickersComp = mconcat $ zipWith compare
                    (reverse $ sort kickers1) (reverse $ sort kickers2)
      pairs1 = map fst $ Map.toList $ Map.filter (== 2) freqs1
      pairs2 = map fst $ Map.toList $ Map.filter (== 2) freqs2
      kickers1 = map fst $ Map.toList $ Map.filter (/= 2) freqs1
      kickers2 = map fst $ Map.toList $ Map.filter (/= 2) freqs2
      freqs1 = rankFreqs h1
      freqs2 = rankFreqs h2
  compare (TwoPair h1) _ = GT
  compare _ (TwoPair h2) = LT
  compare (Pair h1) (Pair h2) = mappend pairsComp kickersComp
    where
      pairsComp = mconcat $ zipWith compare
                  (reverse $ sort pairs1) (reverse $ sort pairs2)
      kickersComp = mconcat $ zipWith compare
                    (reverse $ sort kickers1) (reverse $ sort kickers2)
      pairs1 = map fst $ Map.toList $ Map.filter (== 2) freqs1
      pairs2 = map fst $ Map.toList $ Map.filter (== 2) freqs2
      kickers1 = map fst $ Map.toList $ Map.filter (/= 2) freqs1
      kickers2 = map fst $ Map.toList $ Map.filter (/= 2) freqs2
      freqs1 = rankFreqs h1
      freqs2 = rankFreqs h2
  compare (Pair h1) _ = GT
  compare _ (Pair h2) = LT
  compare (HighCard h1) (HighCard h2) = compare h1 h2

data Winner = Player1 | Player2 | Tie deriving (Show, Eq)


firstKeyWithVal :: (Ord k, Eq v) => Map.Map k v -> v -> k
firstKeyWithVal m val = fst. head . Map.toList $ Map.filter (== val) m

parseLine :: String -> (Hand, Hand)
parseLine line = (Hand (take 5 cards), Hand (drop 5 cards))
  where
    cards = map parseCard $ words line

parseCard :: String -> Card
parseCard (rank:suit:[]) = Card (parseRank [rank]) (parseSuit suit)
parseCard _ = error "Invalid card"

parseSuit :: Char -> Suit
parseSuit 'C' = Clubs
parseSuit 'S' = Spades
parseSuit 'H' = Hearts
parseSuit 'D' = Diamonds
parseSuit _ = error "Invalid suit"

parseRank :: String -> Rank
parseRank ('A':[]) = Rank 14
parseRank ('K':[]) = Rank 13
parseRank ('Q':[]) = Rank 12
parseRank ('J':[]) = Rank 11
parseRank ('T':[]) = Rank 10
parseRank  r = Rank $ read r

cards :: Hand -> [Card]
cards (Hand cs) = cs

rank :: Card -> Rank
rank (Card r s) = r

rankVal :: Rank -> Integer
rankVal (Rank v) = v

ranks :: Hand -> [Rank]
ranks (Hand cs) = map rank cs

suit :: Card -> Suit
suit (Card r s) = s

suits :: Hand -> [Suit]
suits (Hand cs) = map suit cs

pokerHandConverters :: [Hand -> Maybe PokerHand]
pokerHandConverters =
  [toHighCard, toPair, toTwoPair, toThreeOfAKind, toStraight, toFlush,
   toFullHouse, toFourOfAKind, toStraightFlush, toRoyalFlush]

toPokerHand :: Hand -> PokerHand
toPokerHand h = fromJust . maximum $ filter (/= Nothing) phs
  where
    phs = map ($ h) pokerHandConverters

rankFreqs :: Hand -> Map.Map Rank Integer
rankFreqs h = foldl' f Map.empty $ ranks h
  where
    f m r = Map.insertWith (+) r 1 m

toHighCard :: Hand -> Maybe PokerHand
toHighCard h = Just $ HighCard h

toNOfAKind :: Hand -> Integer -> (Hand -> PokerHand) -> Maybe PokerHand
toNOfAKind h n ctr = Map.foldlWithKey f Nothing $ rankFreqs h
  where
    f (Just ph) k v = Just ph
    f Nothing k v =
      if v >= n
      then Just $ ctr h
      else Nothing

toPair :: Hand -> Maybe PokerHand
toPair = flip (`toNOfAKind` 2) Pair

toTwoPair :: Hand -> Maybe PokerHand
toTwoPair h =
  if Map.size pairs == 2
  then Just $ TwoPair h
  else Nothing
    where
      pairs = Map.filter (== 2) freqs
      freqs = rankFreqs h

toThreeOfAKind :: Hand -> Maybe PokerHand
toThreeOfAKind = flip (`toNOfAKind` 3) ThreeOfAKind

toStraight :: Hand -> Maybe PokerHand
toStraight h =
  if allDistinct && correctBounds
  then Just $ Straight h
  else Nothing
    where
      allDistinct = length (nub rs) == 5
      correctBounds = (rankVal (maximum rs) - rankVal (minimum rs)) == 4
      rs = ranks h

toFlush :: Hand -> Maybe PokerHand
toFlush h =
  if matchingSuits
  then Just $ Flush h
  else Nothing
    where
      matchingSuits = (length . nub $ suits h) == 1

toFullHouse :: Hand -> Maybe PokerHand
toFullHouse h =
  if (not $ Map.null trips) && (not $ Map.null pair)
  then Just $ FullHouse h
  else Nothing
    where
      trips = Map.filter (== 3) freqs
      pair = Map.filter (== 2) freqs
      freqs = rankFreqs h

toFourOfAKind :: Hand -> Maybe PokerHand
toFourOfAKind = flip (`toNOfAKind` 4) FourOfAKind

toStraightFlush :: Hand -> Maybe PokerHand
toStraightFlush h =
  if allDistinct && correctBounds && matchingSuits
  then Just $ StraightFlush h
  else Nothing
    where
      allDistinct = (length $ nub rs) == 5
      correctBounds = (rankVal (maximum rs) - rankVal (minimum rs)) == 4
      rs = ranks h
      matchingSuits = (length . nub $ suits h) == 1

toRoyalFlush :: Hand -> Maybe PokerHand
toRoyalFlush h =
  if suitsMatch && correctRanks
  then Just RoyalFlush
  else Nothing
    where
      suitsMatch = (length . nub $ suits h) == 1
      correctRanks = (sort $ ranks h) == (map Rank [10..14])

handWinner :: Hand -> Hand -> Winner
handWinner h1 h2 =
  case compare (toPokerHand h1) (toPokerHand h2) of
    LT -> Player2
    GT -> Player1
    EQ -> Tie

-- Test Data
myStraight :: Hand
myStraight = Hand [Card (Rank 12) Clubs, Card (Rank 10) Spades,
                   Card (Rank 14) Hearts, Card (Rank 13) Diamonds,
                   Card (Rank 11) Spades]

myRoyalFlush :: Hand
myRoyalFlush = Hand [Card (Rank 12) Clubs, Card (Rank 10) Clubs,
                     Card (Rank 14) Clubs, Card (Rank 13) Clubs,
                     Card (Rank 11) Clubs]

my4OfAKind :: Hand
my4OfAKind = Hand [Card (Rank 11) Clubs, Card (Rank 14) Clubs,
                   Card (Rank 11) Clubs, Card (Rank 11) Clubs,
                   Card (Rank 11) Clubs]

myFullHouse :: Hand
myFullHouse = Hand [Card (Rank 4) Spades, Card (Rank 4) Clubs,
                    Card (Rank 4) Hearts, Card (Rank 11) Clubs,
                    Card (Rank 11) Diamonds]

main :: IO ()
main = do
  fileLines <- liftM lines $ readFile "input_files/poker.txt"
  print $ firstPlayerWins $ winners fileLines
  --mapM_ print $ winners fileLines
  where
    nobodyWins = filter (== Tie)
    firstPlayerWins = length . filter (== Player1)
    winners = map (uncurry handWinner . parseLine)
