module Day07
  ( solve
  ) where

import qualified Text.Parsec as P
import Data.List (sort, sortBy, nub)
import Data.Char (isDigit)

import Helpers

type Card     = Char
type Bid      = Int
type Hand     = [Card]
type HandBid  = (Hand, Bid)

data HandType = HighCard | Pair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind deriving (Eq, Show, Ord)

compareCards :: Card -> Card -> Ordering
compareCards a b
  | a == b                  = EQ
  | isDigit a && isDigit b  = compare a b
  | isDigit a               = LT
  | isDigit b               = GT
  | a == 'T'                = LT
  | a == 'J' && b == 'Q'    = LT
  | a == 'J' && b == 'K'    = LT
  | a == 'J' && b == 'A'    = LT
  | a == 'Q' && b == 'K'    = LT
  | a == 'Q' && b == 'A'    = LT
  | a == 'K' && b == 'A'    = LT
  | otherwise               = GT

compareCards2 :: Card -> Card -> Ordering
compareCards2 a b
  | a == b    = EQ
  | a == 'J'  = LT
  | b == 'J'  = GT
  | otherwise = compareCards a b

compareHands :: Hand -> Hand -> Ordering
compareHands a@(a1:as) b@(b1:bs)
  | handType a /= handType b  = compare (handType a) (handType b)
  | otherwise                 = compare' a b
  where compare' (a:as) (b:bs)  | a == b    = compare' as bs
                                | otherwise = compareCards a b

compareHands2 :: Hand -> Hand -> Ordering
compareHands2 a@(a1:as) b@(b1:bs)
  | handType2 a /= handType2 b  = compare (handType2 a) (handType2 b)
  | otherwise                   = compare' a b
  where compare' (a:as) (b:bs)  | a == b    = compare' as bs
                                | otherwise = compareCards2 a b
handType :: Hand -> HandType
handType hand
  | mt 5                                = FiveKind
  | mt 4                                = FourKind
  | md 1                                = FourKind
  | mt 3 && md 3                        = FullHouse
  | mt 2 && md 2                        = FullHouse
  | mt 3                                = ThreeKind
  | m (t 3 $ d 1 h)                     = ThreeKind
  | md 2                                = ThreeKind
  | mt 2 && m (t 2 $ d 2 h)             = TwoPair
  | mt 2 && m (d 3 h)                   = TwoPair
  | m (t 2 (d 1 h)) && m (t 2 (d 3 h))  = TwoPair
  | mt 2                                = Pair
  | m (t 2 $ d 1 h)                     = Pair
  | m (t 2 $ d 2 h)                     = Pair
  | m (t 2 $ d 3 h)                     = Pair
  | otherwise                           = HighCard
  where h     = sort hand
        m xs  = length (nub xs) == 1
        t     = take
        d     = drop
        mt n  = m (t n h)
        md n   = m (d n h)

handType2 :: Hand -> HandType
handType2 hand
  | js == 0                     = handType hand
  | og == HighCard              = Pair
  | og == Pair                  = ThreeKind
  | og == TwoPair && js == 1    = FullHouse
  | og == TwoPair && js == 2    = FourKind
  | og == ThreeKind             = FourKind
  | og == FullHouse && js == 1  = FourKind
  | og == FullHouse && js >= 2  = FiveKind
  | og == FourKind              = FiveKind
  | otherwise                   = FiveKind
  where og  = handType hand
        js  = length $ filter (=='J') hand

parseLine :: Parser HandBid
parseLine = do
  hand <- P.count 5 P.anyChar
  P.space
  bid <- parseInt
  P.newline
  return (hand, bid)

dayParser :: Parser [HandBid]
dayParser = P.many parseLine

part1 :: [HandBid] -> IO ()
part1 input = print $ sum $ map winnings ranked
  where f (h1,b1) (h2,b2)   = compareHands h1 h2
        ranked              = zip [1..] $ sortBy f input
        winnings (r, (_,b)) = r * b

part2 :: [HandBid] -> IO ()
part2 input = print $ sum $ map winnings ranked
  where f (h1,b1) (h2,b2)   = compareHands2 h1 h2
        ranked              = zip [1..] $ sortBy f input
        winnings (r, (_,b)) = r * b

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
