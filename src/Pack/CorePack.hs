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

name = "CorePack"
version = "0.0.1"

keyCallback :: GLFWkeyfun
keyCallback w key scancode action mods = do
  println $ "you pressed the key with number " ++ show key

mouseCallback :: GLFWmousebuttonfun
mouseCallback w button action mods = do
  println $ "you pressed mouse button with number " ++ show button

updateCallback :: WindowInfo -> IO ()
updateCallback windowInfo = do

  let triangle = Triangle (vec3 (-1) 0 0) (vec3 1 0 0) (vec3 0 1 0)
  (dat, renderer) <- RVF.renderVertFragDefault (pure c_GL_TRIANGLES) RVF.vertexSizeColor setAttributePointersColor (pure "color")
  RVF.addTriangle dat triangle (vec3 1 1 0)

  (Registry.renderer.>Registry.push) Registry.RenderLifetimeOneDraw Registry.RenderTransformationNone renderer $
    Just $ Registry.RenderDataProvider (Just $ \ shader _ -> do
      (shader.>SU.setMat4) "V" (identity n4) False
      (shader.>SU.setMat4) "P" (identity n4) False
    ) Nothing Nothing
  pure ()

init reg = do

  Registry.addKeyCallback reg keyCallback
  Registry.addMouseCallback reg mouseCallback
  Registry.addUpdateCallback reg updateCallback


  println "core pack init !"

deinit reg = do
  println "core pack deinit !"

pack = Registry.Pack name version init deinit
