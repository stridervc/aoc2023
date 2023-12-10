module Day10
  ( solve
  ) where

import qualified Text.Parsec as P
import qualified Data.Map.Strict as M

import Helpers
import qualified Grid2D as G

type Pipe       = Char
type Pipes      = [ Pipe ]
type PipesGrid  = G.Grid Pipe

parseRow :: Parser Pipes
parseRow = P.manyTill P.anyChar P.newline

dayParser :: Parser PipesGrid
dayParser = do
  rows <- P.many parseRow
  return $ gridToMap rows

addRow :: Int -> Pipes -> PipesGrid -> PipesGrid
addRow y pipes pg = foldl addPipe pg (zip [0..] pipes)
  where addPipe pg (x, pipe)  = G.putTile (x,y) pipe pg

gridToMap :: [Pipes] -> PipesGrid
gridToMap grid = foldl addrow pg $ zip [0..] grid
  where addrow pm (y,pipes) = addRow y pipes pm
        pg                  = G.newGrid (Just '.')

startCoord :: PipesGrid -> G.Coord
startCoord pg = head $ G.findTiles (=='S') pg

startPipe :: PipesGrid -> Pipe
startPipe pg
  | nc && ec  = 'L'
  | nc && sc  = '|'
  | nc && wc  = 'J'
  | ec && wc  = '-'
  | sc && ec  = 'F'
  | sc && wc  = '7'
  where (x,y) = startCoord pg
        np    = G.getTile (x,y-1) pg
        sp    = G.getTile (x,y+1) pg
        ep    = G.getTile (x+1,y) pg
        wp    = G.getTile (x-1,y) pg
        nc    = np == '|' || np == '7' || np == 'F'
        sc    = sp == '|' || sp == 'J' || sp == 'L'
        ec    = ep == '-' || ep == 'J' || ep == '7'
        wc    = wp == '-' || wp == 'F' || wp == 'L'

loopLength :: Int -> G.Coord -> G.Coord -> PipesGrid -> Int
loopLength count prev curr@(x,y) pg
  | curr == start && count > 0  = count
  | otherwise                   = loopLength (count+1) curr next pg
  where start = startCoord pg
        cp    | curr == start = startPipe pg
              | otherwise     = G.getTile curr pg
        nexts | cp == 'L' = [ (x,y-1), (x+1,y) ]
              | cp == '|' = [ (x,y-1), (x,y+1) ]
              | cp == 'J' = [ (x,y-1), (x-1,y) ]
              | cp == '-' = [ (x-1,y), (x+1,y) ]
              | cp == 'F' = [ (x+1,y), (x,y+1) ]
              | cp == '7' = [ (x-1,y), (x,y+1) ]
        next  | head nexts == prev  = nexts !! 1
              | otherwise           = head nexts

part1 :: PipesGrid -> IO ()
part1 pg = print $ loopLength 0 (0,0) start pg `div` 2
  where start = startCoord pg

part2 :: a -> IO ()
part2 input = putStrLn "n/a"

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
