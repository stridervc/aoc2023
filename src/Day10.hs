module Day10
  ( solve
  ) where

import qualified Text.Parsec as P
import qualified Data.Map.Strict as M

import Helpers

type Coord    = (Int, Int)
type Pipe     = Char
type Pipes    = [ Pipe ]
type PipesMap = M.Map Coord Pipe

parseRow :: Parser Pipes
parseRow = P.manyTill P.anyChar P.newline

dayParser :: Parser PipesMap
dayParser = do
  rows <- P.many parseRow
  return $ gridToMap rows

addRow :: Int -> Pipes -> PipesMap -> PipesMap
addRow y pipes pm = foldl addPipe pm (zip [0..] pipes)
  where addPipe pm (x, pipe)  | pipe == '.' = pm
                              | otherwise   = M.insert (x,y) pipe pm

getCoord :: Coord -> PipesMap -> Pipe
getCoord = M.findWithDefault '.'

startCoord :: PipesMap -> Coord
startCoord pm = head $ M.keys $ M.filter (=='S') pm

startPipe :: PipesMap -> Pipe
startPipe pm
  | nc && ec  = 'L'
  | nc && sc  = '|'
  | nc && wc  = 'J'
  | ec && wc  = '-'
  | sc && ec  = 'F'
  | sc && wc  = '7'
  where (x,y) = startCoord pm
        np    = getCoord (x,y-1) pm
        sp    = getCoord (x,y+1) pm
        ep    = getCoord (x+1,y) pm
        wp    = getCoord (x-1,y) pm
        nc    = np == '|' || np == '7' || np == 'F'
        sc    = sp == '|' || sp == 'J' || sp == 'L'
        ec    = ep == '-' || ep == 'J' || ep == '7'
        wc    = wp == '-' || wp == 'F' || wp == 'L'

gridToMap :: [Pipes] -> PipesMap
gridToMap grid = foldl addrow mempty $ zip [0..] grid
  where addrow pm (y,pipes) = addRow y pipes pm

loopLength :: Int -> Coord -> Coord -> PipesMap -> Int
loopLength count prev curr@(x,y) pm
  | curr == start && count > 0  = count
  | otherwise                   = loopLength (count+1) curr next pm
  where start = startCoord pm
        cp    | curr == start = startPipe pm
              | otherwise     = getCoord curr pm
        nexts | cp == 'L' = [ (x,y-1), (x+1,y) ]
              | cp == '|' = [ (x,y-1), (x,y+1) ]
              | cp == 'J' = [ (x,y-1), (x-1,y) ]
              | cp == '-' = [ (x-1,y), (x+1,y) ]
              | cp == 'F' = [ (x+1,y), (x,y+1) ]
              | cp == '7' = [ (x-1,y), (x,y+1) ]
        next  | head nexts == prev  = nexts !! 1
              | otherwise           = head nexts

part1 :: PipesMap -> IO ()
part1 pm = print $ loopLength 0 (0,0) start pm `div` 2
  where start = startCoord pm

part2 :: a -> IO ()
part2 input = putStrLn "n/a"

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
