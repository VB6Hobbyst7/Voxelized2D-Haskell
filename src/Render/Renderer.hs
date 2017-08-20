{-# LANGUAGE Strict #-}

module Render.Renderer where

import Registry(WindowInfo)
import qualified Registry as Reg
import qualified Defaults as Def
import Graphics.OpenGL
import Common
import Math.Linear.Vec
import qualified Data.HashMap as HashMap


sysDraw :: WindowInfo -> IO ()
sysDraw windowInfo = do
  glClear c_GL_COLOR_BUFFER_BIT
  glClearColor (Def.backGroundColor |> x) (Def.backGroundColor |> y) (Def.backGroundColor |> z) 1

  glfwSwapBuffers (windowInfo |> Reg.windowId)


drawUI :: WindowInfo -> IO ()
drawUI windowInfo = do
  pure ()