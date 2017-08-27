module Memory.VoxelGrid2 where

import Data.Array.IO
import Common
import Math.Linear.Vec
import Math.Nat
import Math.Geometry.Square2

data VoxelGrid2 a = VoxelGrid2{
  a :: a,
  sizeX :: Int,
  sizeY :: Int,
  grid :: IOArray Int a --do not access an element of the array before it is initialized
}


mkGrid :: (Num a) => a -> Int -> Int -> IO (VoxelGrid2 a) --TODO initialization
mkGrid a sizeX sizeY = do
  grid <- newArray (0, (sizeX + 1) * (sizeY + 1) - 1) 0
  pure $ VoxelGrid2 a sizeX sizeY grid

verticesX grid = grid.>sizeX + 1
verticesY grid = grid.>sizeY + 1

get :: VoxelGrid2 a -> Int -> Int -> IO a
get vg x y = readArray (vg.>grid) (y * (vg.>verticesX) + x)

set :: VoxelGrid2 a -> Int -> Int -> a -> IO ()
set vg x y val = writeArray (vg.>grid) (y * (vg.>verticesX) + x ) val

getPoint :: (Num a) => VoxelGrid2 a -> Int -> Int -> Vec N2 a
getPoint vg x y = vec2 ( (vg.>a) * fromIntegral x ) ( (vg.>a) * fromIntegral y ) 

foreachVertex :: (Num a) => VoxelGrid2 a -> (Vec N2 a -> a -> IO ()) -> IO ()
foreachVertex vg fun =
  cfor 0 ( pure (< vg.>verticesY) ) (+1) $ \ y ->
    cfor 0 (pure (< vg.>verticesX) ) (+1) $ \ x -> do
      sample <- get vg x y
      fun (getPoint vg x y) sample

square2 :: (Floating a) => VoxelGrid2 a -> Int -> Int -> Square2 a
square2 vg x y =
  let center = vec2 ((fromIntegral x + 0.5) * (vg.>a)) ((fromIntegral y + 0.5) * (vg.>a))
  in
    Square2 center (vg.>a / 2)