{-# LANGUAGE Strict #-}

module Render.Renderer where

import qualified WindowInfo
import WindowInfo(WindowInfo)
import qualified Registry as Reg
import Registry
import qualified Defaults as Def
import Graphics.OpenGL
import Common
import Math.Linear.Vec
import Data.IORef
import Graphics.Render.RenderVertFrag
import Graphics.Shader.ShaderUtils(Shader)
import qualified Graphics.Shader.ShaderUtils as SU
import Data.Maybe
import Registry(RenderTransformation(..), RenderLifetime(..))
import qualified Data.HashTable.IO as H
import qualified Memory.ArrayBuffer as ArrayBuffer

import Control.Lens hiding (element)

sysDraw :: IORef WindowInfo -> HashTable String Shader -> IO ()
sysDraw _windowInfo shaders = do
  windowInfo <- readIORef _windowInfo

  glClear c_GL_COLOR_BUFFER_BIT
  glClearColor (x Def.backGroundColor) (y Def.backGroundColor) (z Def.backGroundColor) 1

  drawUI _windowInfo shaders

  glfwSwapBuffers (windowInfo^.WindowInfo.handle)


drawUI :: IORef WindowInfo -> HashTable String Shader -> IO ()
drawUI _windowInfo shaders = do
  windowInfo <- readIORef _windowInfo
  --lifetime : one draw
  (Just onetime) <- readIORef Reg.lifetimeOneDrawRenderers
  ArrayBuffer.mapM_ (\val -> do -- == foreach do
    let render = fst val
    name <- shaderName render
    (Just shader) <- (H.lookup) shaders name
    SU.enable shader

    let provider = snd val
    if isJust (provider^.applyShaderData) then do
      let (Just applySData) = provider^.applyShaderData
      applySData shader windowInfo
    else
      pure ()

    if isJust (provider^.applyPreRenderState) then do
      let (Just applyPreState) = provider^.applyPreRenderState
      applyPreState
    else
      pure ()


    construct render
    draw render
    SU.disable shader
    deconstruct render

    if isJust (provider^.applyPostRenderState) then do
      let (Just applyPostState) = provider^.applyPostRenderState
      applyPostState
    else
      pure ()

    SU.disable shader

    ) onetime

  newBuffer <- ArrayBuffer.new 8
  writeIORef Reg.lifetimeOneDrawRenderers (Just newBuffer) --reset the table after rendering

  --lifetime : manual

  
  (Just manual) <- readIORef Reg.lifetimeManualRenderers
  --println $ show manual
  ArrayBuffer.mapM_ (\val -> do -- == foreach do
    let render = fst val
    name <- shaderName render
    (Just shader) <- (H.lookup) shaders name
    SU.enable shader

    let provider = snd val
    if isJust (provider^.applyShaderData) then do
      let (Just applySData) = provider^.applyShaderData
      applySData shader windowInfo
    else
      pure ()

    if isJust (provider^.applyPreRenderState) then do
      let (Just applyPreState) = provider^.applyPreRenderState
      applyPreState
    else
      pure ()


    --render.>construct          construction is on user's side
    draw render
    SU.disable shader
    --render.>deconstruct        same for deconstruction

    if isJust (provider^.applyPostRenderState) then do
      let (Just applyPostState) = provider^.applyPostRenderState
      applyPostState
    else
      pure ()

    SU.disable shader

    ) manual
