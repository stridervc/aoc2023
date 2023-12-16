module Day15
  ( solve
  ) where

import qualified Text.Parsec as P
import Data.List.Split (splitOn)
import Data.Char (ord)

import Helpers

type Hash   = Int
type Step   = String
type Steps  = [ Step ]

dayParser :: Parser Steps
dayParser = do
  line <- P.manyTill P.anyChar P.newline
  return $ splitOn "," line

hash :: Step -> Hash
hash = hash' 0
  where hash' i []      = i
        hash' i (c:cs)  = hash' (((i + ord c) * 17) `mod` 256) cs

part1 :: Steps -> IO ()
part1 input = print $ sum $ map hash input

part2 :: a -> IO ()
part2 input = putStrLn "n/a"

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
