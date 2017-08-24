module Memory.VoxelGrid2 where

import Data.Array.IO
import Common
import Math.Linear.Vec
import Math.Nat

data VoxelGrid2 a = VoxelGrid2{
  a :: a,
  sizeX :: Int,
  sizeY :: Int,
  grid :: IOArray Int a --do not access an element of the array before it is initialized
}


mkGrid :: VoxelGrid2 a -> IO (IOArray Int a)
mkGrid grid = newArray_ (0,grid.>verticesX * grid.>verticesY)

verticesX grid = grid.>sizeX + 1
verticesY grid = grid.>sizeY + 1

get :: VoxelGrid2 a -> Int -> Int -> IO a
get vg x y = readArray (vg.>grid) (y * (vg.>verticesX) + x)

set :: VoxelGrid2 a -> Int -> Int -> a -> IO ()
set vg x y val = writeArray (vg.>grid) (y * (vg.>verticesX) + x ) val

getPoint :: (Num a) => VoxelGrid2 a -> Int -> Int -> Vec N2 a
getPoint vg x y = vec2 ( (vg.>a) * fromIntegral x ) ( (vg.>a) * fromIntegral y ) 

foreachVertex :: (Num a) => VoxelGrid2 a -> (Vec N2 a -> a -> IO ()) -> IO ()
foreachVertex vg fun = do
  cfor 0 ( pure (< vg.>verticesY) ) (+1) $ \ y ->
    cfor 0 (pure (< vg.>verticesX) ) (+1) $ \ x -> do
      sample <- get vg x y
      fun (getPoint vg x y) sample
