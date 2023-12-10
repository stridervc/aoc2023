module Main where

import qualified Data.Map as M
import System.Environment (getArgs)

import qualified Day01 (solve)
import qualified Day02 (solve)
import qualified Day03 (solve)
import qualified Day04 (solve)
import qualified Day05 (solve)
import qualified Day06 (solve)
import qualified Day07 (solve)
import qualified Day08 (solve)
import qualified Day09 (solve)
import qualified Day10 (solve)
import qualified Day11 (solve)

solutions :: M.Map String (String -> IO ())
solutions = M.fromList
  [ ("01", Day01.solve)
  , ("02", Day02.solve)
  , ("03", Day03.solve)
  , ("04", Day04.solve)
  , ("05", Day05.solve)
  , ("06", Day06.solve)
  , ("07", Day07.solve)
  , ("08", Day08.solve)
  , ("09", Day09.solve)
  , ("10", Day10.solve)
  , ("11", Day11.solve)
  ]

solveSingle :: String -> IO ()
solveSingle day = do
  putStrLn $ "--- Day " ++ day ++ " ---"
  case M.lookup day solutions of
    Just solver -> solver day >> putStrLn ""
    Nothing     -> putStrLn "Not yet implemented"

main :: IO ()
main = do
  args <- getArgs
  if null args
    then solveSingle $ last $ M.keys solutions
    else mapM_ solveSingle args
