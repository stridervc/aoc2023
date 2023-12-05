module Day05
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Data.List (transpose, nub)

import Helpers

data AlmanacMap = AlmanacMap
  { mapName         :: String
  , mapSources      :: [Int]
  , mapDestinations :: [Int]
  , mapLengths      :: [Int]
  } deriving (Eq, Show)

data Almanac = Almanac
  { seeds :: [Int]
  , maps  :: [AlmanacMap]
  } deriving (Eq, Show)

seeds2 :: Almanac -> [Int]
seeds2 a = concatMap expand $ pairs $ seeds a
  where pairs []        = []
        pairs (a:b:xs)  = (a,b) : pairs xs
        expand (s,l)    = [s..s+l-1]

newAlmanacMap :: String -> [[Int]] -> AlmanacMap
newAlmanacMap name vals = AlmanacMap name sources dests lengths
  where dests   = head $ transpose vals
        sources = transpose vals !! 1
        lengths = transpose vals !! 2

parseMapLine :: Parser [Int]
parseMapLine = do
  dest <- parseInt
  P.spaces
  source <- parseInt
  P.spaces
  len <- parseInt
  P.newline
  return [dest, source, len]

parseMap :: Parser AlmanacMap
parseMap = do
  name <- P.manyTill P.anyChar P.space
  P.string "map:"
  P.newline
  lines <- P.manyTill parseMapLine (P.newline <|> (P.eof >> return '\n'))
  return $ newAlmanacMap name lines

dayParser :: Parser Almanac
dayParser = do
  P.string "seeds:"
  seeds <- P.manyTill (P.space >> parseInt) P.newline
  P.newline
  maps <- P.manyTill parseMap P.eof
  return $ Almanac seeds maps

convert :: Int -> AlmanacMap -> Int
convert src am
  | fits      = d + src - s
  | otherwise = src
  where ranges          = zip3 (mapDestinations am) (mapSources am) (mapLengths am)
        inrange (_,s,l) = src >= s && src < s+l
        fits            = any inrange ranges
        (d,s,_)         = head $ filter inrange ranges

convertAll :: Almanac -> Int -> Int
convertAll a src = foldl convert src $ maps a

part1 :: Almanac -> IO ()
part1 input = print $ minimum $ map (convertAll input) (seeds input)

part2 :: Almanac -> IO ()
--part2 input = print $ minimum $ map (convertAll input) (seeds2 input)
part2 input = do
  print $ length $ seeds2 input

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
