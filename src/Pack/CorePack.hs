{-# LANGUAGE Strict #-}

module Pack.CorePack where

import Prelude hiding (init)
import Graphics.OpenGL

import qualified Registry
import Common
import Registry(WindowInfo(..))

import Math.Geometry.Triangle
import Math.Linear.Vec
import Math.Linear.Mat
import Math.Nat
import Graphics.Render.RenderVertFrag as RVF
import Graphics.Shader.ShaderUtils as SU
import Data.Global
import Data.IORef
import Math.Collision
import qualified Memory.ArrayBuffer as ArrayBuffer
import Memory.ArrayBuffer(ArrayBuffer(..))
import Math.Geometry.Line2
import Math.Geometry.Square2
import TypeClass.Vector
import qualified Memory.VoxelGrid2 as VG
import Memory.VoxelGrid2(VoxelGrid2(..))
import GHC.IOArray
import Data.Bits

name = "CorePack"
version = "0.0.1"

keyCallback :: GLFWkeyfun
keyCallback w key scancode action mods = do
  println $ "you pressed the key with number " ++ show key

mouseCallback :: GLFWmousebuttonfun
mouseCallback w button action mods = do
  println $ "you pressed mouse button with number " ++ show button

updateCallback :: IORef WindowInfo -> IO ()
updateCallback windowInfo = do
  pure ()

declareIORef "renderer"        --global mutable state ! Good way !!
  [t| Maybe RenderVertFrag |]
  [e| Nothing  |]


type DenFun = Vec2 Float -> IO Float

--TODO remove
placeholderHugeF = 1000000000 :: Float

constSign :: Float -> Float -> Bool
constSign a b
  | a > 0 = b > 0
  | otherwise = b <= 0

calcQEF :: Vec2 Float -> ArrayBuffer Int (Line2 Float) -> IO Float
calcQEF point lines = do
  qef <- newIORef 0 :: IO (IORef Float)
  ArrayBuffer.mapM_ (\ line -> do
    let dist = distancePoint2Line2 point line
    modifyIORef qef ((dist * dist) +)
    ) lines
  readIORef qef

sampleQEF :: Square2 Float -> Int -> ArrayBuffer Int (Line2 Float) -> IO (Vec2 Float)
sampleQEF square n lines = do
  let ext = vec2 (square.>extent) (square.>extent)
  let min = (square.>center) |-| ext
  bestQEF <- newIORef placeholderHugeF --TODO placeholder
  bestPoint <- newIORef min
  cfor 0 (pure $ (<) n) (+1) $ \i ->
    cfor 0 (pure $ (<) n) (+1) $ \j -> do
      let point = min |+| ext |*| vec2 ((2 * fromIntegral i + 1) / fromIntegral n) ((2 * (fromIntegral j) + 1) / fromIntegral n)
      qef <- calcQEF point lines
      curQEF <- readIORef bestQEF
      if qef < curQEF then do
        (bestQEF.>writeIORef) qef
        (bestPoint.>writeIORef) point
      else
        pure ()

  readIORef bestPoint

sampleIntersection :: Line2 Float -> Int -> DenFun -> IO (Vec2 Float)
sampleIntersection line n f = do
  let ext = (line.>end) |-| (line.>start)

  --TODO placeholder
  bestAbs <- newIORef placeholderHugeF
  bestPoint <- newIORef (Nothing :: Maybe (Vec2 Float))

  cfor 0 (pure $ (<) n) (+1) $ \i -> do
    let point = (line.>start) |+| ext |* (fromIntegral i / fromIntegral n)
    den <- f point
    let _abs = abs den

    curBest <- readIORef bestAbs
    if _abs < curBest then do
      (bestAbs.>writeIORef) _abs
      (bestPoint.>writeIORef) (Just point)
    else
      pure ()

  readIORef bestPoint >>= (\(Just x) -> pure x)


sampleTangent :: Square2 Float -> Int -> DenFun -> IO (Vec2 Float)
sampleTangent square n f = do
  let ext = vec2 (square.>extent) (square.>extent)
  let min = (square.>center) |-| ext

  denAtCenter <- f (square.>center)

  --TODO placeholder
  closest <- newIORef (denAtCenter + placeholderHugeF)
  closestPoint <- newIORef (square.>center)

  cfor 0 (pure $ (<) n) (+1) $ \i ->
    cfor 0 (pure $ (<) n) (+1) $ \j -> do
      let point = min |+| ext |*| vec2 (2 * fromIntegral i / fromIntegral n) (2 * fromIntegral j / fromIntegral n)
      den <- f point
      let attempt = abs (den - denAtCenter)
      curClosest <- readIORef closest
      if attempt < curClosest && (point |-| square.>center).>smag /= 0  then do
        (closest.>writeIORef) attempt
        (closestPoint.>writeIORef) point
      else
        pure ()

  resultClosestPoint <- readIORef closestPoint
  pure $ resultClosestPoint |-| square.>center

extForNormal :: Float -> Float
extForNormal !blockSize = blockSize / 100 --TODO why so ?

--returns (featureVertex, intersections, extraPoints)
makeVertex :: VoxelGrid2 Float -> ArrayBuffer (Triangle Float) -> Int -> Int -> DenFun -> Int -> IOArray Int Float -> IO ( Vec2 Float,  ArrayBuffer (Vec2 Float), ArrayBuffer (Vec2 Float) )
makeVertex vg tr x y f accuracy features = do
  p0 <- (vg.>VG.get) x y
  p1 <- (vg.>VG.get) (x+1) y
  p2 <- (vg.>VG.get) x (y+1)
  p3 <- (vg.>VG.get) (x+1) (y+1)

  v0 <- (vg.>VG.getPoint) x y
  v1 <- (vg.>VG.getPoint) (x+1) y
  v2 <- (vg.>VG.getPoint) x (y+1)
  v3 <- (vg.>VG.getPoint) (x+1) (y+1)

  sit <- newIORef (0 :: Int)

  if not constSign p0 p1 then modifyIORef sit $ (.|.) 1 else pure ()
  if not constSign p1 p3 then modifyIORef sit $ (.|.) 2 else pure ()
  if not constSign p3 p2 then modifyIORef sit $ (.|.) 4 else pure ()
  if not constSign p2 p0 then modifyIORef sit $ (.|.) 8 else pure ()


  let _extForNormal = extForNormal (grid.>VG.a)

  _sit <- readIORef sit
  if _sit > 0 then do
    tangents <- ArrayBuffer.new 4 :: IO (ArrayBuffer Int (Line2 Float))
    vert1 <- newIORef (Nothing :: Vec2 Float)
    vert2 <- newIORef (Nothing :: Vec2 Float)

    if (_sit .&. 1) > 0 then do
      ip <- sampleIntersection (Line2 v0 v1) accuracy f
      let full = if p0 <= 0 then v0 else v1
      --TODO render intersection
      dir <- sampleTangent (Square2 ip _extForNormal) accuracy f
      let line = Line2 ip |-| dir / _extForNormal $ ip |+| dir / _extForNormal
      (tangents.>ArrayBuffer.push) line
      --TODO continue

init reg _win = do

  win <- readIORef _win

  let width = fromIntegral $ win.>windowWidth :: Float
  let height = fromIntegral $ win.>windowHeight :: Float

  Registry.addKeyCallback reg keyCallback
  Registry.addMouseCallback reg mouseCallback
  Registry.addUpdateCallback reg updateCallback

  let triangle = Triangle (vec3  0 height 0) (vec3 width height 0) (vec3 (width/2) 0 0)
  (dat, _renderer) <- RVF.renderVertFragDefault (pure c_GL_TRIANGLES) RVF.vertexSizeColor setAttributePointersColor (pure "color")
  writeIORef renderer (Just _renderer)
  RVF.addTriangle dat triangle (vec3 1 1 0)

  _renderer.>RVF.construct

  (Registry.renderer.>Registry.push) Registry.RenderLifetimeManual Registry.RenderTransformationUI _renderer $
    Just $ Registry.RenderDataProvider (Just $ \ shader _ -> do
      (shader.>SU.setMat4) "V" (identity n4) False
      (shader.>SU.setMat4) "P" (identity n4) False
    ) Nothing Nothing



  println "core pack init !"

deinit reg win = do

  (renderer.>readIORef) >>= (\(Just x) -> do x.>RVF.deconstruct;x.>RVF.free)

  println "core pack deinit !"

pack = Registry.Pack name version init deinit
