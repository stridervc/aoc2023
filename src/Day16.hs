module Day16
  ( solve
  ) where

import qualified Text.Parsec as P
import qualified Data.Set as S
import Control.Monad.State

import Helpers
import qualified Grid2D as G

type Contraption  = G.Grid Char
data Direction    = North | South | East | West deriving (Eq, Show, Ord)
type Beam         = (G.Coord, Direction)

data ContraptionState = ContraptionState
  { contraption :: Contraption
  , energised   :: S.Set G.Coord
  , beamsToDo   :: S.Set Beam
  , beamsDone   :: S.Set Beam
  } deriving (Eq, Show)

newContraptionState :: Beam -> Contraption -> ContraptionState
newContraptionState start contr = ContraptionState
  { contraption = contr
  , energised   = mempty
  , beamsToDo   = S.singleton start
  , beamsDone   = mempty
  }

-- add beam to queue, unless (it's been done, or it's out of bounds)
queueBeam :: Beam -> State ContraptionState ()
queueBeam beam = do
  done <- gets beamsDone
  todo <- gets beamsToDo
  (minx, miny, maxx, maxy) <- getMinMaxXY

  if x < minx || x > maxx || y < miny || y > maxy || beam `S.member` done
    then return ()
    else modify (\s -> s { beamsToDo = S.insert beam todo })
  where ((x,y),_) = beam

-- are there still beams to do?
beamsLeft :: State ContraptionState Bool
beamsLeft = gets (not . S.null . beamsToDo)

-- get the next beam to do
-- remove it from to do
-- add it to done (even though it's in progress)
popBeam :: State ContraptionState Beam
popBeam = do
  done <- gets beamsDone
  todo <- gets beamsToDo
  let beam = S.findMin todo
  modify (\s -> s { beamsToDo = S.deleteMin todo, beamsDone = S.insert beam done })
  return beam

-- mark a position as energised
energise :: G.Coord -> State ContraptionState ()
energise pos = do
  gised <- gets energised
  modify (\s -> s { energised = S.insert pos gised })

getTile :: G.Coord -> State ContraptionState Char
getTile pos = gets (G.getTile pos . contraption)

getMinMaxXY :: State ContraptionState (Int, Int, Int, Int)
getMinMaxXY = do
  contr <- gets contraption
  return (G.minX contr, G.minY contr, G.maxX contr, G.maxY contr)

-- process a beam on the contraption, marking energised
-- queues new beams if it's at a splitter
-- queues new beam for next position
doBeam :: Beam -> State ContraptionState ()
doBeam (pos, dir) = do
  tile <- getTile pos
  energise pos
  case tile of
    '.' -> case dir of
            North -> north
            South -> south
            East  -> east
            West  -> west
    '/' -> case dir of
            North -> east
            South -> west
            East  -> north
            West  -> south
    '\\'-> case dir of
            North -> west
            South -> east
            East  -> south
            West  -> north
    '|' -> case dir of
            North -> north
            South -> south
            East  -> north >> south
            West  -> north >> south
    '-' -> case dir of
            North -> east >> west
            South -> east >> west
            East  -> east
            West  -> west
  where (x, y)      = pos
        north       = queueBeam ((x, y-1), North)
        south       = queueBeam ((x, y+1), South)
        east        = queueBeam ((x+1, y), East)
        west        = queueBeam ((x-1, y), West)

-- process all beams
doBeams :: State ContraptionState ()
doBeams = do
  more <- beamsLeft
  when more (popBeam >>= doBeam >> doBeams)

{-
doNBeams :: Int -> State ContraptionState ()
doNBeams 0  = return ()
doNBeams n  = do
  more <- beamsLeft
  when more (popBeam >>= doBeam)
  doNBeams (n-1)

printContraptionState :: ContraptionState -> IO ()
printContraptionState state = do
  G.print id contr
  putStrLn $ "energised = " <> show (energised state)
  putStrLn $ "beamsToDo = " <> show (beamsToDo state)
  putStrLn $ "beamsDone = " <> show (beamsDone state)
  where contr = contraption state
-}

dayParser :: Parser Contraption
dayParser = G.parseCharGrid (Just '.')

part1 :: Contraption -> IO ()
part1 input = print $ S.size $ energised $ execState doBeams cs
  where cs  = newContraptionState ((0,0), East) input

part2 :: Contraption -> IO ()
part2 input = print $ maximum $ map (S.size . energised) states
  where (minx, miny)  = (G.minX input, G.minY input)
        (maxx, maxy)  = (G.maxX input, G.maxY input)
        coords        = [ ((x, miny), South) | x <- [minx..maxx] ]
                      <> [ ((x, maxy), North) | x <- [minx..maxx] ]
                      <> [ ((minx, y), East) | y <- [miny..maxy] ]
                      <> [ ((maxx, y), West) | y <- [miny..maxy] ]
        css           = map (`newContraptionState` input) coords
        states        = map (execState doBeams) css

solve :: String -> IO ()
solve day = do
  parsed <- parseInput day dayParser
  part1 parsed
  part2 parsed
