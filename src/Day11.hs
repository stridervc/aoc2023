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

insertCol :: Int -> Sky -> Sky
insertCol col sky = emptyCol $ foldl shiftRow sky ys
  where xs                  = reverse [col..G.maxX sky]
        ys                  = [G.minY sky..G.maxY sky]
        shiftTile sky (x,y) = G.putTile (x+1,y) (G.getTile (x,y) sky) sky
        shiftRow sky y      = foldl (\sky x -> shiftTile sky (x,y)) sky xs
        emptyCol sky        = foldl (\sky y -> G.putTile (col,y) '.' sky) sky ys

insertRow :: Int -> Sky -> Sky
insertRow row sky = emptyRow $ foldl shiftCol sky xs
  where xs                  = [G.minX sky..G.maxX sky]
        ys                  = reverse [row..G.maxY sky]
        shiftTile sky (x,y) = G.putTile (x,y+1) (G.getTile (x,y) sky) sky
        shiftCol sky x      = foldl (\sky y -> shiftTile sky (x,y)) sky ys
        emptyRow sky        = foldl (\sky x -> G.putTile (x,row) '.' sky) sky xs

isEmptyCol :: Int -> Sky -> Bool
isEmptyCol col sky = all (=='.') tiles
  where tiles = map (\y -> G.getTile (col,y) sky) [G.minY sky..G.maxY sky]

isEmptyRow :: Int -> Sky -> Bool
isEmptyRow row sky = all (=='.') tiles
  where tiles = map (\x -> G.getTile (x,row) sky) [G.minX sky..G.maxX sky]

expandCols :: Sky -> Sky
expandCols sky = foldr insertCol sky cols
  where cols  = filter (`isEmptyCol` sky) [G.minX sky..G.maxX sky]

expandRows :: Sky -> Sky
expandRows sky = foldr insertRow sky rows
  where rows  = filter (`isEmptyRow` sky) [G.minY sky..G.maxY sky]

galaxies :: Sky -> [G.Coord]
galaxies sky = filter isGalaxy [(x,y) | x <- [G.minX sky..G.maxX sky], y <- [G.minY sky..G.maxY sky]]
  where isGalaxy pos  = G.getTile pos sky == '#'

expand :: Sky -> Sky
expand = expandCols . expandRows

manhattan :: G.Coord -> G.Coord -> Int
manhattan (x1,y1) (x2,y2) = lx - sx + ly - sy
  where (lx, sx)  | x1 > x2   = (x1,x2)
                  | otherwise = (x2,x1)
        (ly, sy)  | y1 > y2   = (y1,y2)
                  | otherwise = (y2,y1)
part1 :: Sky -> IO ()
part1 sky = print $ sum $ map (uncurry manhattan) $ pairs $ galaxies $ expand sky

part2 :: Sky -> IO ()
-- part2 sky = print $ sum $ map (uncurry manhattan) $ pairs $ galaxies $ expand2 1000000 sky
part2 _ = putStrLn "n/a"

solve :: String -> IO ()
solve day = do
  parsed <- parseTestInput day dayParser
  part1 parsed
  part2 parsed
