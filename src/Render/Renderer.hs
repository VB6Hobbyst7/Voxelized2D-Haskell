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

sysDraw :: WindowInfo -> HashTable String Shader -> IO ()
sysDraw windowInfo shaders = do
  glClear c_GL_COLOR_BUFFER_BIT
  glClearColor (Def.backGroundColor.>x) (Def.backGroundColor.>y) (Def.backGroundColor.>z) 1

  drawUI windowInfo shaders

  glfwSwapBuffers (windowInfo |> Reg.windowId)


drawUI :: WindowInfo -> HashTable String Shader -> IO ()
drawUI windowInfo shaders = do
  onetime <- readIORef Reg.lifetimeOneDrawRenderers
  H.mapM_ (\(_,val) -> do -- == foreach do
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
    render.>deconstruct

    if isJust (Reg.applyPostRenderState provider) then do
      let (Just applyPostState) = Reg.applyPostRenderState provider
      applyPostState
    else
      pure ()

    SU.disable shader

    ) onetime