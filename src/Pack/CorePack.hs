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

init reg _win = do

  win <- readIORef _win

  let width = fromIntegral $ win.>windowWidth :: Float
  let height = fromIntegral $ win.>windowHeight :: Float

  Registry.addKeyCallback reg keyCallback
  Registry.addMouseCallback reg mouseCallback
  Registry.addUpdateCallback reg updateCallback

  let triangle = Triangle (vec3  0 height 0) (vec3 width height 0) (vec3 (width/2) height 0)
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
