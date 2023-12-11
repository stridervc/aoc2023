module Helpers
  ( Parser
  , readInput
  , readTestInput
  , parseInput
  , parseTestInput
  , parseNegInt
  , parseInt
  , parseInts
  , pairs
  ) where

import qualified Text.Parsec as P
import qualified Text.Parsec.String as SP

import Text.Parsec.String (Parser)

-- generate input filename from day
inputFileName :: String -> String
inputFileName day = "./inputs/input" <> day

-- generate test input filename from day
testFileName :: String -> String
testFileName day = "./inputs/test" <> day

-- read input file for day
readInput :: String -> IO String
readInput day = readFile $ inputFileName day

-- read test input for day
readTestInput :: String -> IO String
readTestInput day = readFile $ testFileName day

-- parse input file for day
parseInput :: String -> Parser a -> IO a
parseInput day parser = do
  result <- SP.parseFromFile parser $ inputFileName day
  case result of
    Left e        -> error $ show e
    Right parsed  -> return parsed

-- parse test input file for day
parseTestInput :: String -> Parser a -> IO a
parseTestInput day parser = do
  result <- SP.parseFromFile parser $ testFileName day
  case result of
    Left e        -> error $ show e
    Right parsed  -> return parsed

-- parse a negative int
parseNegInt :: Integral a => Read a => Parser a
parseNegInt = do
  P.char '-'
  num <- P.many1 P.digit
  return $ read num * (-1)

-- parse an Int
parseInt :: Integral a => Read a => Parser a
parseInt = P.choice
  [ parseNegInt
  , do
    num <- P.many1 P.digit
    return $ read num
  ]

-- parse newline terminated rows of Ints
parseInts :: Parser [Int]
parseInts = P.many1 $ do
  num <- parseInt
  P.newline
  return num

-- generate all pairs of list items
pairs :: [a] -> [(a,a)]
pairs []      = []
pairs [a,b]   = [(a,b)]
pairs (a:xs)  = [(a,x) | x <- xs] <> pairs xs
