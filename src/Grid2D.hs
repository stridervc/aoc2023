module Grid2D
  ( Grid (..)
  , Coord
  , newGrid
  , putTile
  , getTile
  , findTiles
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust, fromJust)

type Coord  = (Int, Int)

data Grid a = Grid
  { grid  :: M.Map Coord a
  , def   :: Maybe a
  } deriving (Eq, Show)

newGrid :: Maybe a -> Grid a
newGrid def = Grid
  { grid  = mempty
  , def   = def
  }

-- place a tile in the grid, unless the tile is a default tile
putTile :: Eq a => Coord -> a -> Grid a -> Grid a
putTile coord tile g
  | isJust mdef && tile == def'   = g
  | otherwise                     = g { grid = M.insert coord tile grid' }
  where mdef  = def g
        def'  = fromJust mdef
        grid' = grid g

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
