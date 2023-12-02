module Day02
  ( solve
  ) where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))

import Helpers

type Cubes  = (Int, Char)
type Draw   = [ Cubes ]
type Game   = (Int, [ Draw ])
type Games  = [ Game ]

parseCubes :: Parser Cubes
parseCubes = do
  P.spaces
  amount <- parseInt
  P.space
  color <- P.string "red" <|> P.string "green" <|> P.string "blue"
  return (amount, head color)

parseDraw :: Parser Draw
parseDraw = P.sepBy parseCubes (P.char ',')

parseDraws :: Parser [ Draw ]
parseDraws = P.sepBy parseDraw (P.char ';')

parseGame :: Parser Game
parseGame = do
  P.string "Game "
  game <- parseInt
  P.char ':'
  draws <- parseDraws
  P.newline
  return (game, draws)

dayParser :: Parser Games
dayParser = P.many parseGame

isValidCubes :: Cubes -> Bool
isValidCubes (count, cube)
  | cube == 'r' = count <= 12
  | cube == 'g' = count <= 13
  | cube == 'b' = count <= 14
  | otherwise   = error $ "Unexpected cube " <> show cube

isValidGame :: Game -> Bool
isValidGame (game, draws) = all isValidDraw draws
  where isValidDraw = all isValidCubes

part1 :: Games -> IO ()
part1 input = print $ sum $ map fst $ filter isValidGame input

minReqCubes :: Game -> (Int, Int, Int)
minReqCubes (game, draws) = (max 'r', max 'g', max 'b')
  where getCount cube []          = 0
        getCount cube ((i,c):ds)  | c == cube = i
                                  | otherwise = getCount cube ds
        max cube                  = maximum $ map (getCount cube) draws

part2 :: Games -> IO ()
part2 input = print $ sum $ map (power . minReqCubes) input
  where power (r,g,b) = r*g*b

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
