module Graphics.Render.RenderVertFrag where


import qualified Memory.MemBlock as Mem

import Graphics.OpenGL
import Math.Geometry.Triangle
import Math.Linear.Vec
import Math.Nat
import Common

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr (Ptr,nullPtr)
import Control.Monad
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as UC

data RenderVertFrag = RenderVertFrag
 {
    renderMode :: Int,
    setAttributePointers :: IO (),
    construct :: Data -> IO (Bool,Data),
    deconstruct :: Data -> IO (Bool,Data),
    draw :: Data -> IO Bool,
    shaderName :: String
  }

type Data = RenderVertFragDataDefault

data RenderVertFragDataDefault = RenderVertFragData
  {
    vertexSize :: Int,
    vertexPool :: Mem.MemBlock CFloat,
    indexPool :: Mem.MemBlock CInt,
    vertexCount :: Int,
    vao :: Int,
    vbo :: Int,
    ebo :: Int,
    constructed :: Bool
  }


vertexSizeColor = 6 :: Int

setAttributePointersColor :: IO ()
setAttributePointersColor =
  let vertexSize = vertexSizeColor
      stride = 4 * vertexSize
  in do
      glVertexAttribPointer 0 3 c_GL_FLOAT False stride 0
      glEnableVertexAttribArray 0

      glVertexAttribPointer 1 3 c_GL_FLOAT False stride (3 * 4)
      glEnableVertexAttribArray 1


addTriangle :: Data -> Triangle Float -> Vec N3 Float -> IO Data
addTriangle (RenderVertFragData vs vp ip vc vao vbo ebo constr) triangle color = do
  vp <- Mem.add vp $ realToFrac(x $ p1 triangle)
  vp <- Mem.add vp $ realToFrac(y $ p1 triangle)
  vp <- Mem.add vp $ realToFrac(z $ p1 triangle)

  vp <- Mem.add vp $ realToFrac $ x color
  vp <- Mem.add vp $ realToFrac $ y color
  vp <- Mem.add vp $ realToFrac $ z color

  vp <- Mem.add vp $ realToFrac(x $ p2 triangle)
  vp <- Mem.add vp $ realToFrac(y $ p2 triangle)
  vp <- Mem.add vp $ realToFrac(z $ p2 triangle)

  vp <- Mem.add vp $ realToFrac $ x color
  vp <- Mem.add vp $ realToFrac $ y color
  vp <- Mem.add vp $ realToFrac $ z color

  vp <- Mem.add vp $ realToFrac(x $ p3 triangle)
  vp <- Mem.add vp $ realToFrac(y $ p3 triangle)
  vp <- Mem.add vp $ realToFrac(z $ p3 triangle)

  vp <- Mem.add vp $ realToFrac $ x color
  vp <- Mem.add vp $ realToFrac $ y color
  vp <- Mem.add vp $ realToFrac $ z color

  ip <- Mem.add ip 0
  ip <- Mem.add ip 1
  ip <- Mem.add ip 2

  pure (RenderVertFragData vs vp ip vc vao vbo ebo constr)

renderVertFragDefault :: Int -> Int -> IO () -> String -> IO (Data, RenderVertFrag)
renderVertFragDefault mode vertSize setAttribPointers shaderName = do
    vp <- Mem.new 8 :: IO (Mem.MemBlock CFloat)
    ip <- Mem.new 8 :: IO (Mem.MemBlock CInt)
    let dat = RenderVertFragData vertSize vp ip 0 0 0 0 False

    let
      construct :: Data -> IO (Bool, Data)
      construct dat@(RenderVertFragData vertSize vp ip vc _ _ _ _) =
          if not $ constructed dat then do

            vao <- glGenVertexArrays
            vbo <- glGenBuffers
            ebo <- glGenBuffers

            glBindVertexArray vao
            glBindBuffer c_GL_ARRAY_BUFFER vbo
            glBufferData c_GL_ARRAY_BUFFER (4 * (Mem.stored $ vertexPool dat)) (castPtr $ Mem.ptr $ vertexPool dat) c_GL_STATIC_DRAW

            glBindBuffer c_GL_ELEMENT_ARRAY_BUFFER ebo
            glBufferData c_GL_ELEMENT_ARRAY_BUFFER (4 * (Mem.stored $ indexPool dat)) (castPtr $ Mem.ptr $ indexPool dat) c_GL_STATIC_DRAW


            setAttribPointers

            --glBindBuffer c_GL_ARRAY_BUFFER 0
            --glBindVertexArray 0 TODO
            pure (True, RenderVertFragData vertSize vp ip vc vao vbo ebo True)
          else
            pure (False,dat)

      deconstruct :: Data -> IO (Bool,Data)
      deconstruct dat@(RenderVertFragData vertSize vp ip vc vao vbo ebo _) =
        if constructed dat then do
          glDeleteVertexArrays vao
          glDeleteBuffers vbo
          glDeleteBuffers ebo
          pure (True, RenderVertFragData vertSize vp ip vc vao vbo ebo False)
        else
          pure (False,dat)

      draw :: Data -> IO Bool
      draw dat =
        if constructed dat then do
          glBindVertexArray $ vao dat
          glDrawElements mode (Mem.stored $ indexPool dat) c_GL_UNSIGNED_INT nullPtr
          glBindVertexArray 0
          pure True
        else
          pure False

      render = RenderVertFrag mode setAttribPointers construct deconstruct draw shaderName

    pure (dat, render)
