module Day03
  ( solve
  ) where

import qualified Text.Parsec as P

import Data.Char (isDigit)
import Data.Maybe (catMaybes)

import Helpers

type Schematic  = [[Char]]
type Coord      = (Int, Int)

parseLine :: Parser [Char]
parseLine = P.manyTill P.anyChar P.newline

dayParser :: Parser Schematic
dayParser = P.many parseLine

getXY :: Schematic -> Coord -> Char
getXY schem (x,y)
  | x > maxx || y > maxy  = '.'
  | otherwise             = schem !! y !! x
  where maxx  = length (head schem) - 1
        maxy  = length schem - 1

hasAdjSymbol :: Schematic -> Coord -> Bool
hasAdjSymbol schem (x,y)  = any isSymbol adj
  where isSymbol pos    = not (getXY schem pos == '.' || isDigit (getXY schem pos))
        adj             = [(ix, iy) | ix <- [x-1..x+1], ix >= 0, iy <- [y-1..y+1], iy >= 0, ix <= maxx, iy <= maxy]
        maxx            = length (head schem) - 1
        maxy            = length schem - 1

isPart :: Schematic -> Coord -> Bool
isPart schem pos@(x,y)
  | not (isDigit c)     = False
  | otherwise           = hasAdjSymbol schem pos || (hasNX && isPart schem (x+1,y))
  where c     = getXY schem pos
        pc    = getXY schem (x-1,y)
        hasNX = x < maxx
        maxx  = length (head schem) - 1

readStr :: Schematic -> Coord -> String
readStr schem pos@(x,y)
  | not (isDigit c) = []
  | otherwise       = c : readStr schem (x+1,y)
  where c   = getXY schem pos

readPart :: Schematic -> Coord -> Maybe Int
readPart schem pos@(x,y)
  | not (isPart schem pos)  = Nothing
  | x > 0 && isDigit pc     = Nothing
  | otherwise               = Just (read str)
  where str   = readStr schem pos
        pc    = getXY schem (x-1, y)

part1 :: Schematic -> IO ()
part1 schem = print $ sum parts
  where parts = catMaybes [ readPart schem (x,y) | x <- [0..maxx], y <- [0..maxy] ]
        maxx  = length (head schem) - 1
        maxy  = length schem - 1

part2 :: Schematic -> IO ()
part2 input = putStrLn "n/a"

solve :: String -> IO ()
solve day = do
  parsed <- parseTestInput day dayParser
  part1 parsed
  part2 parsed
