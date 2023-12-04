module Day04
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Data.IntMap.Strict as IM

import Helpers

type Card     = (Int, [Int], [Int])
type Cards    = [Card]
type Winners  = IM.IntMap [Int]

parseCard :: Parser Card
parseCard = do
  P.string "Card"
  P.spaces
  no <- parseInt
  P.char ':'
  P.spaces
  winning <- P.manyTill parseInt' (P.char '|')
  numbers <- P.manyTill (P.spaces >> parseInt) P.newline
  return (no, winning, numbers)
  where parseInt' = do
          P.spaces
          i <- parseInt
          P.spaces
          return i

dayParser :: Parser Cards
dayParser = P.many parseCard

points :: Card -> Int
points (_, winning, nums)
  | n == 0    = 0
  | otherwise = 2 ^ (n-1)
  where n = length $ filter (`elem` winning) nums

part1 :: Cards -> IO ()
part1 cards = print $ sum $ map points cards

markWinners :: Card -> Winners -> Winners
markWinners (no, winning, nums) = IM.insert no ids
  where n   = length $ filter (`elem` winning) nums
        ids = tail [no..no+n]

-- a bit slow, memoisation would help
counts :: Winners -> Int -> Int
counts _ 1  = 1
counts w i  = 1 + sum (map (counts w) wcards)
  where wcards  = IM.keys $ IM.filter (elem i) w

part2 :: Cards -> IO ()
part2 cards = print $ sum $ map (counts winners) [1..length cards]
  where winners = foldr markWinners mempty cards

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
