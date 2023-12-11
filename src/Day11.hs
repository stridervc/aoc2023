module Day11
  ( solve
  ) where

import qualified Text.Parsec as P

import Helpers
import qualified Grid2D as G

type Sky = G.Grid Char

dayParser :: Parser Sky
dayParser = do
  grid <- P.many parseLine
  return $ G.newGridFromList (Just '.') grid
  where parseLine = P.manyTill P.anyChar P.newline

isEmptyCol :: Int -> Sky -> Bool
isEmptyCol col sky = all (=='.') tiles
  where tiles = map (\y -> G.getTile (col,y) sky) [G.minY sky..G.maxY sky]

isEmptyRow :: Int -> Sky -> Bool
isEmptyRow row sky = all (=='.') tiles
  where tiles = map (\x -> G.getTile (x,row) sky) [G.minX sky..G.maxX sky]

galaxies :: Sky -> [G.Coord]
galaxies sky = filter isGalaxy [(x,y) | x <- [G.minX sky..G.maxX sky], y <- [G.minY sky..G.maxY sky]]
  where isGalaxy pos  = G.getTile pos sky == '#'

manhattan :: G.Coord -> G.Coord -> Int
manhattan (x1,y1) (x2,y2) = lx - sx + ly - sy
  where (lx, sx)  = (max x1 x2, min x1 x2)
        (ly, sy)  = (max y1 y2, min y1 y2)

emptyColsBetween :: Sky -> G.Coord -> G.Coord -> Int
emptyColsBetween sky (x1,_) (x2,_) = length $ filter (`isEmptyCol` sky) [sx..lx]
  where lx  = max x1 x2
        sx  = min x1 x2

emptyRowsBetween :: Sky -> G.Coord -> G.Coord -> Int
emptyRowsBetween sky (_,y1) (_,y2) = length $ filter (`isEmptyRow` sky) [sy..ly]
  where ly  = max y1 y2
        sy  = min y1 y2

solveIt :: Int -> Sky -> Int
solveIt n sky = sum $ map add $ zip3 mtrows mtcols manhat
  where pairs'      = pairs $ galaxies sky
        mtrows      = map (uncurry $ emptyRowsBetween sky) pairs'
        mtcols      = map (uncurry $ emptyColsBetween sky) pairs'
        manhat      = map (uncurry manhattan) pairs'
        add (r,c,d) = d + r * (n-1) + c * (n-1)

part1 :: Sky -> IO ()
part1 sky = print $ solveIt 2 sky

part2 :: Sky -> IO ()
part2 sky = print $ solveIt 1000000 sky

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
