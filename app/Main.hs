module Main where

import qualified Data.Map as M
import System.Environment (getArgs)

import qualified Day01 (solve)
import qualified Day02 (solve)

solutions :: M.Map String (String -> IO ())
solutions = M.fromList
  [ ("01", Day01.solve)
  , ("02", Day02.solve)
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
    then solveSingle $ fst $ M.findMax solutions
    else mapM_ solveSingle args
