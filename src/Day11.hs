module Day11
  ( solve
  ) where

import qualified Text.Parsec as P

import Helpers

dayParser :: Parser a
dayParser = undefined

part1 :: a -> IO ()
part1 input = putStrLn "n/a"

part2 :: a -> IO ()
part2 input = putStrLn "n/a"

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
