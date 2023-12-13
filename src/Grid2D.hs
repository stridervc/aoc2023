module Grid2D
  ( Grid (..)
  , Coord
  , newGrid
  , newGridFromList
  , putTile
  , getTile
  , findTiles
  , minX
  , maxX
  , minY
  , maxY
  , toList
  , transpose
  , Grid2D.print
  , orthogonalCoords
  , orthogonalTiles
  ) where

import qualified Data.Map.Strict as M
import qualified Data.List as L

import Data.Maybe (isJust, fromJust)

type Coord  = (Int, Int)

data Grid a = Grid
  { grid      :: M.Map Coord a
  , def       :: Maybe a
  , minXCoord :: Maybe Int
  , maxXCoord :: Maybe Int
  , minYCoord :: Maybe Int
  , maxYCoord :: Maybe Int
  } deriving (Eq, Show)

newGrid :: Maybe a -> Grid a
newGrid def = Grid
  { grid      = mempty
  , def       = def
  , minXCoord = Nothing
  , maxXCoord = Nothing
  , minYCoord = Nothing
  , maxYCoord = Nothing
  }

newGridFromList :: Eq a => Maybe a -> [[a]] -> Grid a
newGridFromList def as  = foldl putrow g $ zip [0..] as
  where g               = newGrid def
        putrow g (y,r)  = foldl (\g (x,t) -> putTile (x,y) t g) g $ zip [0..] r

-- place a tile in the grid, unless the tile is a default tile
-- update min and max x and y even if it's a default tile
putTile :: Eq a => Coord -> a -> Grid a -> Grid a
putTile coord@(x,y) tile g
  | isJust mdef && tile == def' && empty  = g'
  | otherwise                             = g' { grid = grid' }
  where mdef  = def g
        def'  = fromJust mdef
        grid' = M.insert coord tile $ grid g
        minx  = case minXCoord g of
                  Nothing -> x
                  Just x' -> min x x'
        maxx  = case maxXCoord g of
                  Nothing -> x
                  Just x' -> max x x'
        miny  = case minYCoord g of
                  Nothing -> y
                  Just y' -> min y y'
        maxy  = case maxYCoord g of
                  Nothing -> y
                  Just y' -> max y y'
        g'    = g { minXCoord = Just minx, maxXCoord = Just maxx, minYCoord = Just miny, maxYCoord = Just maxy }
        empty = M.notMember coord $ grid g

-- get a tile from the grid, returns default if it doesn't exist
getTile :: Coord -> Grid a -> a
getTile coord g
  | isJust mdef = M.findWithDefault def' coord grid'
  | otherwise   = case M.lookup coord grid' of
                    Just a  -> a
                    Nothing -> error $ "getTile coord " <> show coord <> " doesn't exist"
  where mdef  = def g
        def'  = fromJust mdef
        grid' = grid g

-- find coordinates of tiles that match search filter
findTiles :: (a -> Bool) -> Grid a -> [ Coord ]
findTiles filter' g = M.keys $ M.filter filter' $ grid g

minX :: Grid a -> Int
minX g = case minXCoord g of
          Nothing -> error "minX: no tiles placed yet"
          Just x  -> x

maxX :: Grid a -> Int
maxX g = case maxXCoord g of
          Nothing -> error "maxX: no tiles placed yet"
          Just x  -> x

minY :: Grid a -> Int
minY g = case minYCoord g of
          Nothing -> error "minY: no tiles placed yet"
          Just y  -> y

maxY :: Grid a -> Int
maxY g = case maxYCoord g of
          Nothing -> error "maxY: no tiles placed yet"
          Just y  -> y

-- get values as 2d list
toList :: Grid a -> [[a]]
toList g = map row [miny..maxy]
  where minx  = minX g
        maxx  = maxX g
        miny  = minY g
        maxy  = maxY g
        row y = [getTile (x,y) g | x <- [minx..maxx]]

transpose :: Eq a => Grid a -> Grid a
transpose g = newGridFromList def' (L.transpose l)
  where def'  = def g
        l     = toList g

print :: (a -> Char) -> Grid a -> IO ()
print f g = do
  putStrLn $ show (minx, miny) <> " to " <> show (maxx, maxy)
  mapM_ printrow [miny..maxy]
  where minx        = minX g
        maxx        = maxX g
        miny        = minY g
        maxy        = maxY g
        list        = toList g
        row y       = list !! y
        printrow y  = putStrLn $ map f $ row y

orthogonalCoords :: Grid a -> Coord -> [Coord]
orthogonalCoords g (x,y) = filter (`M.member` grid') [ (x-1,y), (x+1,y), (x,y-1), (x,y+1) ]
  where grid' = grid g

orthogonalTiles :: Grid a -> Coord -> [a]
orthogonalTiles g coord = map (`getTile` g) $ orthogonalCoords g coord
