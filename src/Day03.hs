module Day03
  ( solve
  ) where

import qualified Text.Parsec as P

import Data.List (nub)
import Data.Char (isDigit)
import Data.Maybe (catMaybes)

import Helpers

type Schematic  = [[Char]]
type Coord      = (Int, Int)

parseLine :: Parser [Char]
parseLine = P.manyTill P.anyChar P.newline

dayParser :: Parser Schematic
dayParser = P.many parseLine

maxX :: Schematic -> Int
maxX schem = length (head schem) - 1

maxY :: Schematic -> Int
maxY schem = length schem - 1

getXY :: Schematic -> Coord -> Char
getXY schem (x,y)
  | x < 0 || y < 0        = '.'
  | x > maxx || y > maxy  = '.'
  | otherwise             = schem !! y !! x
  where maxx  = maxX schem
        maxy  = maxY schem

hasAdjSymbol :: Schematic -> Coord -> Bool
hasAdjSymbol schem (x,y)  = any isSymbol adj
  where isSymbol pos    = not (getXY schem pos == '.' || isDigit (getXY schem pos))
        adj             = [(ix, iy) | ix <- [x-1..x+1], iy <- [y-1..y+1] ]

isPart :: Schematic -> Coord -> Bool
isPart schem pos@(x,y)
  | not (isDigit c)     = False
  | otherwise           = hasAdjSymbol schem pos || (hasNX && isPart schem (x+1,y))
  where c     = getXY schem pos
        hasNX = x < maxX schem

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

backReadPart :: Schematic -> Coord -> Maybe Int
backReadPart schem pos@(x,y)
  | not (isDigit c)     = Nothing
  | x > 0 && isDigit pc = backReadPart schem (x-1,y)
  | otherwise           = readPart schem pos
  where c   = getXY schem pos
        pc  = getXY schem (x-1,y)

part1 :: Schematic -> IO ()
part1 schem = print $ sum parts
  where parts = catMaybes [ readPart schem (x,y) | x <- [0..maxX schem], y <- [0..maxY schem] ]

adjParts :: Schematic -> Coord -> [Int]
adjParts schem (x,y)  = nub $ catMaybes [ backReadPart schem (nx,ny) | nx <- [x-1..x+1], ny <- [y-1..y+1] ]

isGear :: Schematic -> Coord -> Bool
isGear schem pos  = c == '*' && length (adjParts schem pos) == 2
  where c = getXY schem pos

part2 :: Schematic -> IO ()
part2 schem = print $ sum $ map (ratio . adjParts schem) gears
  where gears     = filter (isGear schem) [ (x,y) | x <- [0..maxX schem], y <- [0..maxY schem] ]
        ratio ps  = head ps * ps !! 1

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
