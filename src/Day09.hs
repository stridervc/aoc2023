module Day09
  ( solve
  ) where

import qualified Text.Parsec as P

import Helpers

dayParser :: Parser [[Int]]
dayParser = P.many parseLine
  where parseLine = P.manyTill (P.spaces >> parseInt) P.newline

diffs :: [Int] -> [Int]
diffs []        = []
diffs [a,b]     = [b - a]
diffs (a:b:xs)  = b - a : diffs (b:xs)

next :: [Int] -> [Int]
next xs
  | all (==0) xs  = xs <> [ 0 ]
  | otherwise     = xs <> [ last xs + last diffs' ]
  where diffs'    = next $ diffs xs

prev :: [Int] -> [Int]
prev xs
  | all (==0) xs  = 0 : xs
  | otherwise     = head xs - head diffs' : xs
  where diffs'    = prev $ diffs xs

part1 :: [[Int]] -> IO ()
part1 input = print $ sum $ map last nexts
  where nexts = map next input

part2 :: [[Int]] -> IO ()
part2 input = print $ sum $ map head prevs
  where prevs = map prev input

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
