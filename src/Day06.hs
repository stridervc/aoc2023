module Day06
  ( solve
  ) where

calc :: (Int, Int) -> Int -> Int
calc (t, wd) hold = hold * (t - hold)

beats :: (Int, Int) -> Int -> Bool
beats (t, wd) hold = calc (t, wd) hold > wd

numbeats :: (Int, Int) -> Int
numbeats (t, wd) = length $ filter (beats (t, wd)) (tries (t, wd))
  where tries (t, _)  = [1..t-1]

part1 :: [(Int, Int)] -> IO ()
part1 input = print $ product $ map numbeats input

part2 :: (Int, Int) -> IO ()
part2 input = print $ numbeats input

solve :: String -> IO ()
solve day = do
  part1 input
  part2 input2
  where input   = [ (54, 302), (94, 1476), (65, 1029), (92, 1404) ]
        input2  = (54946592, 302147610291404)
        test    = [ (7, 9), (15, 40), (30, 200) ]
        test2   = (71530, 940200)
