{-# LANGUAGE Strict #-}

module Render.Renderer where

import Registry(WindowInfo)
import qualified Registry as Reg
import qualified Defaults as Def
import Graphics.OpenGL
import Common
import Math.Linear.Vec
import Data.IORef
import Graphics.Render.RenderVertFrag
import Graphics.Shader.ShaderUtils(Shader)
import qualified Graphics.Shader.ShaderUtils as SU
import Data.Maybe
import Registry(RenderTranformation(..), RenderLifetime(..))
import qualified Data.HashTable.IO as H
import qualified Memory.ArrayBuffer as ArrayBuffer

sysDraw :: IORef WindowInfo -> HashTable String Shader -> IO ()
sysDraw _windowInfo shaders = do
  windowInfo <- readIORef _windowInfo

  glClear c_GL_COLOR_BUFFER_BIT
  glClearColor (Def.backGroundColor.>x) (Def.backGroundColor.>y) (Def.backGroundColor.>z) 1

  drawUI _windowInfo shaders

  glfwSwapBuffers (windowInfo.>Reg.windowId)


drawUI :: IORef WindowInfo -> HashTable String Shader -> IO ()
drawUI _windowInfo shaders = do
  windowInfo <- readIORef _windowInfo
  --lifetime : one draw
  (Just onetime) <- readIORef Reg.lifetimeOneDrawRenderers
  ArrayBuffer.mapM_ (\val -> do -- == foreach do
    let render = fst val
    name <- render.>shaderName
    (Just shader) <- (H.lookup) shaders name
    shader.>SU.enable

    let provider = snd val
    if isJust (Reg.applyShaderData provider) then do
      let (Just applySData) = Reg.applyShaderData provider
      applySData shader windowInfo
    else
      pure ()

    if isJust (Reg.applyPreRenderState provider) then do
      let (Just applyPreState) = Reg.applyPreRenderState provider
      applyPreState
    else
      pure ()


    render.>construct
    render.>draw
    shader.>SU.disable
    render.>deconstruct

    if isJust (Reg.applyPostRenderState provider) then do
      let (Just applyPostState) = Reg.applyPostRenderState provider
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
    name <- render.>shaderName
    (Just shader) <- (H.lookup) shaders name
    shader.>SU.enable

    let provider = snd val
    if isJust (Reg.applyShaderData provider) then do
      let (Just applySData) = Reg.applyShaderData provider
      applySData shader windowInfo
    else
      pure ()

    if isJust (Reg.applyPreRenderState provider) then do
      let (Just applyPreState) = Reg.applyPreRenderState provider
      applyPreState
    else
      pure ()


    --render.>construct          construction is on user's side
    render.>draw
    shader.>SU.disable
    --render.>deconstruct        same for deconstruction

    if isJust (Reg.applyPostRenderState provider) then do
      let (Just applyPostState) = Reg.applyPostRenderState provider
      applyPostState
    else
      pure ()

    SU.disable shader

    ) manual
