module Day14
  ( solve
  ) where

import qualified Text.Parsec as P

import Helpers
import qualified Grid2D as G

type Platform = G.Grid Char

dayParser :: Parser Platform
dayParser = G.parseCharGrid (Just '.')

tiltRow :: [Char] -> [Char]
tiltRow []        = []
tiltRow [a]       = [a]
tiltRow [a,b]     | a == '.' && b == 'O'  = "O."
                  | otherwise             = [a,b]
tiltRow (a:b:xs)  | a == 'O'      = a : tiltRow (b:xs)
                  | a == '#'      = a : tiltRow (b:xs)
                  | b == 'O'      = 'O' : tiltRow ('.':xs)
                  | b == '#'      = ".#" <> tiltRow xs
                  | null rest     = a:b:xs
                  | nondot == '#' = ds <> "#" <> tiltRow (tail rest)
                  | nondot == 'O' = tiltRow ('O' : ds <> tiltRow (tail rest))
  where (ds,rest) = span (=='.') (a:b:xs)
        nondot    = head rest

tiltNorth :: Platform -> Platform
tiltNorth plat = G.transpose $ G.mapToRows tiltRow $ G.transpose plat

scoreRow :: [Char] -> Int
scoreRow row = foldr score 0 $ zip [1..] $ reverse row
  where score (v,c) i | c == 'O'  = i + v
                      | otherwise = i

scorePlatform :: Platform -> Int
scorePlatform plat = sum $ map scoreRow $ G.toList $ G.transpose plat

part1 :: Platform -> IO ()
part1 input = print $ scorePlatform $ tiltNorth input

part2 :: a -> IO ()
part2 input = putStrLn "n/a"

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
