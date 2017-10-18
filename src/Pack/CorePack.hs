{-# LANGUAGE Strict,TypeApplications #-}

module Pack.CorePack where

import Prelude hiding (init, lines)
import Graphics.OpenGL

import qualified Registry
import Common
import Registry(WindowInfo(..))

import Math.Geometry.Triangle
import Math.Geometry.Triangle2
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
import qualified Memory.VoxelGrid2 as VG
import Memory.VoxelGrid2(VoxelGrid2(..))
import GHC.IOArray
import Data.Bits
import qualified Math.Geometry.SShape2 as Solid
import Math.Geometry.SShape2(DenFun)
import Data.Singletons.TypeLits

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

declareIORef "linesRenderer"
  [t|Maybe RenderVertFrag|]
  [e|Nothing|]

_block_size = 0.125 :: Float
_chunk_size = 128 :: Int

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
  cfor' 0 (< n) (+1) $ \i ->
    cfor' 0 (< n) (+1) $ \j -> do
      let point = min |+| ext |*| vec2 ((2 * fromIntegral i + 1) / fromIntegral n) ((2 * (fromIntegral j) + 1) / fromIntegral n)
      qef <- calcQEF point lines
      curQEF <- readIORef bestQEF
      if qef < curQEF then do
        (bestQEF.>writeIORef) qef
        (bestPoint.>writeIORef) point
      else
        pure ()

  readIORef bestPoint

sampleIntersection :: Line2 Float -> Int -> DenFun Float -> IO (Vec2 Float)
sampleIntersection line n f = do
  let ext = (line.>end) |-| (line.>start)

  --TODO placeholder
  bestAbs <- newIORef placeholderHugeF
  bestPoint <- newIORef (Nothing :: Maybe (Vec2 Float))

  cfor' 0 (< n) (+1) $ \i -> do
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


sampleTangent :: Square2 Float -> Int -> DenFun Float -> IO (Vec2 Float)
sampleTangent square n f = do
  let ext = vec2 (square.>extent) (square.>extent)
  let min = (square.>center) |-| ext

  denAtCenter <- f (square.>center)

  --TODO placeholder
  closest <- newIORef (denAtCenter + placeholderHugeF)
  closestPoint <- newIORef (square.>center)

  cfor' 0 (< n) (+1) $ \i ->
    cfor' 0 (< n) (+1) $ \j -> do
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
makeVertex :: VoxelGrid2 Float -> ArrayBuffer Int (Triangle2 Float) -> Int -> Int -> DenFun Float -> Int -> IOArray Int (Maybe (Vec2 Float)) -> ArrayBuffer Int (Vec2 Float) -> ArrayBuffer Int (Vec2 Float) -> IO ( Maybe (Vec2 Float))
makeVertex vg tr x y f accuracy features outIntersections outExtra = do
  p0 <- (vg.>VG.get) x y
  p1 <- (vg.>VG.get) (x+1) y
  p2 <- (vg.>VG.get) x (y+1)
  p3 <- (vg.>VG.get) (x+1) (y+1)

  v0 <- pure $ (vg.>VG.getPoint) x y
  v1 <- pure $ (vg.>VG.getPoint) (x+1) y
  v2 <- pure $ (vg.>VG.getPoint) x (y+1)
  v3 <- pure $ (vg.>VG.getPoint) (x+1) (y+1)

  sit <- newIORef (0 :: Int)

  if not (constSign p0 p1) then modifyIORef sit $ (.|.) 1 else pure ()
  if not (constSign p1 p3) then modifyIORef sit $ (.|.) 2 else pure ()
  if not (constSign p3 p2) then modifyIORef sit $ (.|.) 4 else pure ()
  if not (constSign p2 p0) then modifyIORef sit $ (.|.) 8 else pure ()


  let _extForNormal = extForNormal (vg.>VG.a)

  _sit <- readIORef sit
  if _sit > 0 then do
    tangents <- ArrayBuffer.new 4 :: IO (ArrayBuffer Int (Line2 Float))

    let
      worker :: Int -> Vec2 Float -> Vec2 Float -> Float -> Float -> IO ()
      worker and v_a v_b p_a p_b =
        if (_sit .&. and) > 0 then do
          ip <- sampleIntersection (Line2 v_a v_b) accuracy f
          let full = if p_a <= 0 then v_a else v_b
          --TODO render intersection
          dir <- sampleTangent (Square2 ip _extForNormal) accuracy f
          let line = Line2 (ip |-| dir |* (1 /  _extForNormal)) (ip |+| dir |* (1 /  _extForNormal))
          (tangents.>ArrayBuffer.push) line
          (outIntersections.>ArrayBuffer.push) ip
          (outExtra.>ArrayBuffer.push) full
          pure ()
        else do
          let negative = p_a < 0
          if negative then do
            (outIntersections.>ArrayBuffer.push) v_a
            (outExtra.>ArrayBuffer.push) v_b
            pure ()
          else
            pure ()

    worker 1 v0 v1 p0 p1
    worker 2 v1 v3 p1 p3
    worker 4 v3 v2 p3 p2
    worker 8 v2 v0 p2 p0

    interpolatedVertex <- sampleQEF ((vg.>VG.square2) x y) accuracy tangents

    cfor 0 (\ i -> pure (i <) <*> (outIntersections.>ArrayBuffer.size) ) (+1) $ \i -> do
      _p1 <- (outIntersections.>ArrayBuffer.read) i
      _p2 <- (outExtra.>ArrayBuffer.read) i
      (tr.>ArrayBuffer.push) $ Triangle2 interpolatedVertex _p1 _p2

    (features.>writeIOArray) (y * vg.>sizeX + x ) (Just interpolatedVertex)

    pure $ Just interpolatedVertex
  else
    pure Nothing


data ContourData = ContourData{
  lines :: ArrayBuffer Int (Line2 Float),
  triangles :: ArrayBuffer Int (Triangle2 Float),
  features :: IOArray Int (Maybe (Vec2 Float)),
  intersections :: IOArray Int (Maybe (ArrayBuffer Int (Vec2 Float))),
  extras :: IOArray Int (Maybe (ArrayBuffer Int (Vec2 Float)))
}

makeContour :: VoxelGrid2 Float -> DenFun Float -> Int -> IO ContourData
makeContour vg f accuracy = do
  res1 <- ArrayBuffer.new 128 :: IO (ArrayBuffer Int (Line2 Float))
  res2 <- ArrayBuffer.new 128 :: IO (ArrayBuffer Int (Triangle2 Float))

  features <- newIOArray (0,vg.>sizeX * vg.>sizeY - 1) Nothing :: IO (IOArray Int (Maybe (Vec2 Float)))
  intersections <- newIOArray (0, vg.>sizeX * vg.>sizeY - 1) Nothing :: IO (IOArray Int (Maybe (ArrayBuffer Int (Vec2 Float))) )
  extra <- newIOArray (0, vg.>sizeX * vg.>sizeY - 1) Nothing :: IO (IOArray Int (Maybe (ArrayBuffer Int (Vec2 Float))) )

  let
    cachedMake :: Int -> Int -> IO (Maybe (Vec2 Float))
    cachedMake x y = do
      let t = y * vg.>sizeX + x
      possible <- (features.>readIOArray) t
      case possible of
        (Just x) -> pure $ Just x
        Nothing -> do
          _new1 <- ArrayBuffer.new 4 :: IO (ArrayBuffer Int (Vec2 Float))
          _new2 <- ArrayBuffer.new 4 :: IO (ArrayBuffer Int (Vec2 Float))
          (intersections.>writeIOArray) t (Just _new1)
          (intersections.>writeIOArray) t (Just _new2)

          ret <- makeVertex vg res2 x y f accuracy features _new1 _new2

          case ret of
            Nothing -> do
              (intersections.>writeIOArray) t Nothing
              (extra.>writeIOArray) t Nothing
              pure ()
            _ -> pure ()
          pure ret

  cfor' 0 (< (vg.>VG.sizeY)) (+1) $ \y ->
    cfor' 0 (< (vg.>VG.sizeX)) (+1) $ \x -> do
      p0 <- (vg.>VG.get) x y
      p1 <- (vg.>VG.get) (x+1) y
      p2 <- (vg.>VG.get) x (y+1)
      p3 <- (vg.>VG.get) (x+1) (y+1)

      v0 <- pure $ (vg.>VG.getPoint) x y
      v1 <- pure $ (vg.>VG.getPoint) (x+1) y
      v2 <- pure $ (vg.>VG.getPoint) x (y+1)
      v3 <- pure $ (vg.>VG.getPoint) (x+1) (y+1)

      sit <- newIORef (0 :: Int)

      if not (constSign p0 p1) then modifyIORef sit $ (.|.) 1 else pure ()
      if not (constSign p1 p3) then modifyIORef sit $ (.|.) 2 else pure ()
      if not (constSign p3 p2) then modifyIORef sit $ (.|.) 4 else pure ()
      if not (constSign p2 p0) then modifyIORef sit $ (.|.) 8 else pure ()

      _sit <- readIORef sit

      if _sit > 0 then do
        (Just interpolatedVertex) <- cachedMake x y

        vert1 <- newIORef (Nothing :: Maybe (Vec2 Float))
        vert2 <- newIORef (Nothing :: Maybe (Vec2 Float))

        if (_sit .&. 2) > 0 then
          if (x + 1) < vg.>sizeX then do
            made <- cachedMake (x+1) y
            (vert1.>writeIORef) made
          else pure ()
        else pure ()

        if (_sit .&. 4) > 0 then
          if (y + 1) < vg.>sizeY then do
            made <- cachedMake x (y+1)
            (vert2.>writeIORef) made
          else pure ()
        else pure ()

        --TODO debug render outline

        vert1_ <- vert1.>readIORef
        vert2_ <- vert2.>readIORef

        case vert1_ of
          (Just v) -> do
            (res1.>ArrayBuffer.push) $ Line2 interpolatedVertex v
            pure ()
          Nothing -> pure ()
        case vert2_ of
          (Just v) -> do
            (res1.>ArrayBuffer.push) $ Line2 interpolatedVertex v
            pure ()
          Nothing -> pure ()


        pure ()
      else do
        let negative = p0 < 0
        if negative then do
          let tr1 = Triangle2 v0 v1 v3
          let tr2 = Triangle2 v0 v3 v2

          (res2.>ArrayBuffer.push) tr1
          (res2.>ArrayBuffer.push) tr2
          pure ()
        else pure ()

  pure $ ContourData res1 res2 features intersections extra


fillInGrid :: VoxelGrid2 Float -> DenFun Float -> IO ()
fillInGrid vg f =
 cfor' 0 ( < vg.>VG.verticesY ) (+1) $ \ y ->
    cfor' 0 (< vg.>VG.verticesX ) (+1) $ \ x -> do
      den <- f $ vec2 (vg.>a * fromIntegral x) (vg.>a * fromIntegral y)
      ((vg.>VG.grid).>writeIOArray) (y * vg.>VG.verticesX + x) den  --TODO point = (0,0)


defaultProvider mwin = do
  win <- mwin.>rr'
  let height = 16 :: Float
  let aspect = fromIntegral (win.>windowWidth) / fromIntegral (win.>windowHeight)
  let width = height * aspect
  pure $ Just $ Registry.RenderDataProvider (Just $ \ shader _ -> do
      (shader.>SU.setMat4) "V" (identity (SNat @4)) False
      (shader.>SU.setMat4) "P" (Math.Linear.Mat.ortho 0 width 0 height (-1) 1) False
    ) Nothing Nothing

identityProvider =
  Just $ Registry.RenderDataProvider (Just $ \ shader _ -> do
      (shader.>SU.setMat4) "V" (identity (SNat @4)) False
      (shader.>SU.setMat4) "P" (identity (SNat @4)) False
    ) Nothing Nothing


init reg mwin = do

  win <- readIORef mwin

  let width = fromIntegral $ win.>windowWidth :: Float
  let height = fromIntegral $ win.>windowHeight :: Float

  Registry.addKeyCallback reg keyCallback
  Registry.addMouseCallback reg mouseCallback
  Registry.addUpdateCallback reg updateCallback

  grid <- VG.mkGrid _block_size _chunk_size _chunk_size

  let
    offset = vec2 0.1 (0.1 :: Float)
    circle1 = Solid.circle2 (vec2 4 8 |+| offset) (2 :: Float)
    circle2 = Solid.circle2 (vec2 8 8 |+| offset) (5 :: Float)
    circle3 = Solid.circle2 (vec2 4 4 |+| offset) (2 :: Float)
    circle4 = Solid.circle2 (vec2 8 12 |+| offset) (4 :: Float)
    circle5 = Solid.circle2 (vec2 8 6 |+| offset) (1.1 :: Float)
    rec = Solid.rectangle2 (vec2 8 10.8 |+| offset) (vec2 1 (3 :: Float))
    shape = circle1 `Solid.union` circle2 `Solid.union` rec `Solid.difference` circle3 `Solid.difference` circle4 `Solid.difference` circle5 `Solid.union` rec

  dat <- timed (\dt -> "fillInGrid: " ++ show dt ++ " ms") $ do
    fillInGrid grid shape
    makeContour grid shape 32

  
  linesCount <- (dat.>lines).>ArrayBuffer.size
  println $ "generated " ++ show linesCount ++ " lines"


  (linesData, _linesRenderer) <- RVF.renderVertFragDefault (pure c_GL_LINES) RVF.vertexSizeColor setAttributePointersColor (pure "color")

  

  ArrayBuffer.mapM_ (\line -> do
      (linesData.>addLine2) line 0 (vec3 1 1 1)
    ) (dat.>lines)


  linesRenderer.>rw' ! Just _linesRenderer

  let triangle = Triangle (vec3  0 0 0) (vec3 1 0 0) (vec3 0 1 0)
  (dat, _renderer) <- RVF.renderVertFragDefault (pure c_GL_TRIANGLES) RVF.vertexSizeColor setAttributePointersColor (pure "color")
  writeIORef renderer (Just _renderer)
  RVF.addTriangle dat triangle (vec3 1 1 0)

  _renderer.>RVF.construct
  _linesRenderer.>RVF.construct

  --(Registry.renderer.>Registry.push) Registry.RenderLifetimeManual Registry.RenderTransformationNone _renderer identityProvider

  
  (Registry.renderer.>Registry.push) Registry.RenderLifetimeManual Registry.RenderTransformationNone _linesRenderer =<< defaultProvider mwin


  
  println "core pack init !"

deinit reg win = do

  (renderer.>rr')      >>= (\(Just x) -> do x.>RVF.deconstruct;x.>RVF.free)
  (linesRenderer.>rr') >>= (\(Just x) -> do x.>RVF.deconstruct;x.>RVF.free)

  println "core pack deinit !"

pack = Registry.Pack name version init deinit
