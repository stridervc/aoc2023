module Day01
  ( solve
  ) where

import qualified Text.Parsec as P

import Helpers

import Data.Char (isDigit)

parseLine :: Parser String
parseLine = P.manyTill P.anyChar P.newline

dayParser :: Parser [String]
dayParser = P.many parseLine

calibration :: String -> Int
calibration s = read [ head digits, last digits ]
  where digits  = filter isDigit s

part1 :: [String] -> IO ()
part1 input = print $ sum $ map calibration input

subWords :: String -> String
subWords s
  | length s < 3  = s
  | is "one"      = "1" <> subWords (tail s)
  | is "two"      = "2" <> subWords (tail s)
  | is "three"    = "3" <> subWords (tail s)
  | is "four"     = "4" <> subWords (tail s)
  | is "five"     = "5" <> subWords (tail s)
  | is "six"      = "6" <> subWords (tail s)
  | is "seven"    = "7" <> subWords (tail s)
  | is "eight"    = "8" <> subWords (tail s)
  | is "nine"     = "9" <> subWords (tail s)
  | otherwise     = head s : subWords (tail s)
  where is w      = take (length w) s == w

part2 :: [String] -> IO ()
part2 input = print $ sum $ map (calibration . subWords) input

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
