module Day12
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))

import Data.List (group, elemIndex)
import Data.Maybe (fromJust, isNothing)

import Helpers

type History  = String
type Checksum = [Int]
type Record   = (History, Checksum)
type Records  = [ Record ]

parseLine :: Parser Record
parseLine = do
  hist <- P.manyTill histchar P.space
  check <- P.sepBy parseInt (P.char ',')
  P.newline
  return (hist, check)
  where histchar  = P.char '?' <|> P.char '.' <|> P.char '#'

dayParser :: Parser Records
dayParser = P.many parseLine

isValid :: Record -> Bool
isValid (hist, check) = check == map length (filter (\s -> head s == '#') $ group hist)

-- ...
-- #..
-- .#.
-- ##.
-- ..#
-- #.#
-- .##
-- ###
nextHistory :: Record -> History -> Maybe History
nextHistory (hist,check) curr
  | '?' `elem` curr   = Just $ map (\c -> if c == '?' then '.' else c) curr
  | all (=='#') vals  = Nothing
  | otherwise         = Just $ map (\i -> if hist!!i == '?' then repl i else hist!!i) [0..length hist - 1]
  where qis         = foldr (\i l -> if hist!!i == '?' then i : l else l) [] [0..length hist - 1]
        vals        = map (curr!!) qis
        inc []      = []
        inc (x:xs)  | x == '.'  = '#' : xs
                    | x == '#'  = '.' : inc xs
        nvals       = inc vals
        repl i      = nvals !! fromJust (elemIndex i qis)

possibilities' :: Record -> History -> [ History ]
possibilities' (hist, check) curr
  | isNothing next          = []
  | isValid (jnext, check)  = jnext : possibilities' (hist, check) jnext
  | otherwise               = possibilities' (hist, check) jnext
  where next                = nextHistory (hist, check) curr
        jnext               = fromJust next

possibilities :: Record -> [ History ]
possibilities (hist, check) = possibilities' (hist, check) hist

unfold :: Record -> Record
unfold (hist, check) = (hist', check')
  where hist'   = concat [ hist, ['?'], hist, ['?'], hist, ['?'], hist, ['?'], hist ]
        check'  = concat [ check, check, check, check, check ]

part1 :: Records -> IO ()
part1 input = print $ sum $ map (length . possibilities) input

part2 :: Records -> IO ()
part2 input = putStrLn "n/a"

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
