module Day13
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Data.List (intersect)

import Helpers
import qualified Grid2D as G

type Pattern  = G.Grid Char
type Patterns = [ Pattern ]

parseLine :: Parser [Char]
parseLine = do
  c <- P.char '.' <|> P.char '#'
  cs <- P.manyTill (P.char '.' <|> P.char '#') P.newline
  return (c:cs)

parsePattern :: Parser Pattern
parsePattern = do
  l <- parseLine
  ls <- P.manyTill parseLine (P.newline <|> (P.eof >> return '\n'))
  return $ G.newGridFromList (Just '.') (l:ls)

dayParser :: Parser [Pattern]
dayParser = P.many parsePattern

-- check if point after index is a vertical mirror point
isMirrorPoint :: [Char] -> Int -> Bool
isMirrorPoint line i = take n left == take n right
  where left  = reverse $ take i line
        right = drop i line
        n     = min (length left) (length right)

vertMirrorPoints :: Pattern -> [Int]
vertMirrorPoints patt = inter mps
  where points        = [1..G.maxX patt]
        linemps line  = filter (isMirrorPoint line) points
        mps           = map linemps $ G.toList patt
        inter [a]     = a
        inter [a,b]   = a `intersect` b
        inter (a:b:xs)  = a `intersect` b `intersect` inter xs

horisMirrorPoints :: Pattern -> [Int]
horisMirrorPoints patt = vertMirrorPoints $ G.transpose patt

score :: Pattern -> Int
score patt = v + 100*h
  where v = sum $ vertMirrorPoints patt
        h = sum $ horisMirrorPoints patt

part1 :: Patterns -> IO ()
part1 input = print $ sum $ map score input

part2 :: a -> IO ()
part2 input = putStrLn "n/a"

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
