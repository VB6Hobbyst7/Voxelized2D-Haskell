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

import Data.IORef


--interface for any rendering VAO
data RenderVertFrag = RenderVertFrag{
  renderMode ::           IO Int,
  setAttributePointers :: IO (),
  construct ::            IO Bool,
  deconstruct ::          IO Bool, --for opengl deconstruction
  draw ::                 IO Bool,
  shaderName ::           IO String,
  free ::                 IO ()     --for freeing of RAM buffers
}



--default data for simple VAO
data RenderVertFragDataDefault = RenderVertFragDataDefault{
    vertexPool :: Mem.MemBlock CFloat, --TODO do not forget to free !
    indexPool :: Mem.MemBlock CInt,
    vertexCount :: Int,
    vao :: Int,
    vbo :: Int,
    ebo :: Int,
    constructed :: Bool
}


vertexSizeColor = pure 6 :: IO Int

setAttributePointersColor :: IO ()
setAttributePointersColor = do
  vertexSize <- vertexSizeColor
  let stride = 4 * vertexSize
  glVertexAttribPointer 0 3 c_GL_FLOAT False stride 0
  glEnableVertexAttribArray 0

  glVertexAttribPointer 1 3 c_GL_FLOAT False stride (3 * 4)
  glEnableVertexAttribArray 1



--probably move this function to separate module
addTriangle :: IORef RenderVertFragDataDefault -> Triangle Float -> Vec N3 Float -> IO ()
addTriangle dat triangle color = do
  (RenderVertFragDataDefault vp ip vc vao vbo ebo constr) <- readIORef dat
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

  writeIORef dat $ RenderVertFragDataDefault vp ip vc vao vbo ebo constr

  pure ()

newDefaultData :: IO (IORef RenderVertFragDataDefault)
newDefaultData = do
  vp <- Mem.new 8 :: IO (Mem.MemBlock CFloat)
  ip <- Mem.new 8 :: IO (Mem.MemBlock CInt)
  let dat = RenderVertFragDataDefault vp ip 0 0 0 0 False
  newIORef dat

--return data and renderer. CAUTION ! renderer is bind to the data returned (by reference), changing the data changes how rendering occurs
renderVertFragDefault :: IO Int -> IO Int -> IO () -> IO String -> IO (IORef RenderVertFragDataDefault, RenderVertFrag)
renderVertFragDefault mode vertSize setAttribPointers shaderName = do
    mdat <- newDefaultData

    let
      construct :: IORef RenderVertFragDataDefault -> IO Bool
      construct mdat = do
          dat@(RenderVertFragDataDefault vp ip vc _ _ _ _) <- readIORef mdat
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
            writeIORef mdat $ RenderVertFragDataDefault vp ip vc vao vbo ebo True
            pure True
          else
            pure False

      deconstruct :: IORef RenderVertFragDataDefault -> IO Bool
      deconstruct mdat = do
        dat@(RenderVertFragDataDefault vp ip vc vao vbo ebo _) <- readIORef mdat
        if constructed dat then do
          glDeleteVertexArrays vao
          glDeleteBuffers vbo
          glDeleteBuffers ebo
          writeIORef mdat $ RenderVertFragDataDefault vp ip vc vao vbo ebo False
          pure True
        else
          pure False

      draw :: IORef RenderVertFragDataDefault -> IO Bool
      draw mdat = do
        dat <- readIORef mdat
        if constructed dat then do
          glBindVertexArray $ vao dat
          smode <- mode
          glDrawElements smode (Mem.stored $ indexPool dat) c_GL_UNSIGNED_INT nullPtr
          glBindVertexArray 0
          pure True
        else
          pure False

      free :: IORef RenderVertFragDataDefault -> IO ()
      free mdat = do
        dat <- readIORef mdat
        dat.>vertexPool.>Mem.delete
        dat.>indexPool.>Mem.delete

    let render = RenderVertFrag mode setAttribPointers (construct mdat) (deconstruct mdat) (draw mdat) shaderName (free mdat)--those args should be lazy, TODO verify that !

    pure (mdat, render)
