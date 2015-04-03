module Main where

import           Control.Monad
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid


-- Types --
data Suit = Clubs | Spades | Hearts | Diamonds deriving (Show, Eq, Ord)
data Rank = Rank Integer deriving (Show, Eq, Ord)
data Card = Card Rank Suit deriving (Show, Eq)
data Hand = Hand [Card] deriving (Show, Eq)
data PokerHand = HighCard Kickers | Pair Kickers | TwoPair Kickers |
                 ThreeOfAKind Kickers | Straight Kickers | Flush Kickers |
                 FullHouse Kickers | FourOfAKind Kickers |
                 StraightFlush Kickers | RoyalFlush deriving (Show, Eq, Ord)
newtype Kickers = Kickers { getKickers :: [Rank] } deriving (Show)
data Winner = Player1 | Player2 | Tie deriving (Show, Eq)


-- Instances --
instance Ord Card where
  compare (Card r1 s1) (Card r2 s2) = compare r1 r2

instance Ord Kickers where
  compare k1 k2
    | length k1' == length k2' = mconcat $ zipWith compare k1' k2'
    | otherwise = error "Invalid Kicker comparison"
    where
      k1' = getKickers k1
      k2' = getKickers k2

instance Eq Kickers where
  (==) k1 k2 = compare k1 k2 == EQ


-- Functions --
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

rank :: Card -> Rank
rank (Card r s) = r

ranks :: Hand -> [Rank]
ranks (Hand cs) = map rank cs

rankVal :: Rank -> Integer
rankVal (Rank v) = v

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

getKeysForValues :: (Ord k) => (v -> Bool) -> Map.Map k v -> [k]
getKeysForValues f m = map fst . Map.toList $ Map.filter f m

toHighCard :: Hand -> Maybe PokerHand
toHighCard h = Just $ HighCard $ Kickers $ sortBy (flip compare) $ ranks h

toNOfAKind :: Hand -> Integer -> (Kickers -> PokerHand) -> Maybe PokerHand
toNOfAKind h n ctr =
  if not $ null kindRanks
  then Just $ ctr $ Kickers (kindRanks ++ otherRanks)
  else Nothing
  where
    kindRanks = sortBy (flip compare) $ getKeysForValues (== n) freqs
    otherRanks = sortBy (flip compare) $ getKeysForValues (/= n) freqs
    freqs = rankFreqs h

toPair :: Hand -> Maybe PokerHand
toPair = flip (`toNOfAKind` 2) Pair

toTwoPair :: Hand -> Maybe PokerHand
toTwoPair h =
  if length pairRanks == 2
  then Just $ TwoPair $ Kickers (pairRanks ++ otherRanks)
  else Nothing
  where
    pairRanks = sortBy (flip compare) $ getKeysForValues (== 2) freqs
    otherRanks = sortBy (flip compare) $ getKeysForValues (/= 2) freqs
    freqs = rankFreqs h

toThreeOfAKind :: Hand -> Maybe PokerHand
toThreeOfAKind = flip (`toNOfAKind` 3) ThreeOfAKind

isStraight :: Hand -> Bool
isStraight h = allDistinct && correctBounds
  where
    allDistinct = length (nub rs) == 5
    correctBounds = (rankVal (maximum rs) - rankVal (minimum rs)) == 4
    rs = ranks h

-- doesn't handle ace as low card
toStraight :: Hand -> Maybe PokerHand
toStraight h =
  if isStraight h
  then Just $ Straight $ Kickers [maximum rs]
  else Nothing
    where
      rs = ranks h

toFlush :: Hand -> Maybe PokerHand
toFlush h =
  if matchingSuits
  then Just $ Flush $ Kickers $ sortBy (flip compare) $ ranks h
  else Nothing
    where
      matchingSuits = (length . nub $ suits h) == 1

toFullHouse :: Hand -> Maybe PokerHand
toFullHouse h =
  if not (null trips) && not (null pair)
  then Just $ FullHouse $ Kickers [head trips, head pair, head kicker]
  else Nothing
    where
      trips = getKeysForValues (== 3) freqs
      pair = getKeysForValues (== 2) freqs
      kicker = getKeysForValues (== 1) freqs
      freqs = rankFreqs h

toFourOfAKind :: Hand -> Maybe PokerHand
toFourOfAKind = flip (`toNOfAKind` 4) FourOfAKind

toStraightFlush :: Hand -> Maybe PokerHand
toStraightFlush h =
  if isStraight h && matchingSuits
  then Just $ StraightFlush $ Kickers [maximum rs]
  else Nothing
    where
      rs = ranks h
      matchingSuits = (length . nub $ suits h) == 1

toRoyalFlush :: Hand -> Maybe PokerHand
toRoyalFlush h =
  if suitsMatch && correctRanks
  then Just RoyalFlush
  else Nothing
  where
    suitsMatch = (length . nub $ suits h) == 1
    correctRanks = sort (ranks h) == map Rank [10..14]

handWinner :: Hand -> Hand -> Winner
handWinner h1 h2
  | ph1 > ph2 = Player1
  | ph1 < ph2 = Player2
  | otherwise = Tie
  where
    ph1 = toPokerHand h1
    ph2 = toPokerHand h2

main :: IO ()
main = do
  fileLines <- liftM lines $ readFile "input_files/poker.txt"
  print $ firstPlayerWins $ winners fileLines
  where
    firstPlayerWins = length . filter (== Player1)
    winners = map (uncurry handWinner . parseLine)
