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
import Math.Geometry.Line2

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
    vertexCount :: IORef Int,
    vao :: IORef Int,
    vbo :: IORef Int,
    ebo :: IORef Int,
    constructed :: IORef Bool
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
addTriangle :: IORef RenderVertFragDataDefault -> Triangle Float -> Vec 3 Float -> IO ()
addTriangle mdat triangle color = do
  dat <- readIORef mdat
  let vp = dat.>vertexPool
  let ip = dat.>indexPool
  Mem.add vp $ realToFrac(x $ p1 triangle)
  Mem.add vp $ realToFrac(y $ p1 triangle)
  Mem.add vp $ realToFrac(z $ p1 triangle)

  Mem.add vp $ realToFrac $ x color
  Mem.add vp $ realToFrac $ y color
  Mem.add vp $ realToFrac $ z color

  Mem.add vp $ realToFrac(x $ p2 triangle)
  Mem.add vp $ realToFrac(y $ p2 triangle)
  Mem.add vp $ realToFrac(z $ p2 triangle)

  Mem.add vp $ realToFrac $ x color
  Mem.add vp $ realToFrac $ y color
  Mem.add vp $ realToFrac $ z color

  Mem.add vp $ realToFrac(x $ p3 triangle)
  Mem.add vp $ realToFrac(y $ p3 triangle)
  Mem.add vp $ realToFrac(z $ p3 triangle)

  Mem.add vp $ realToFrac $ x color
  Mem.add vp $ realToFrac $ y color
  Mem.add vp $ realToFrac $ z color

  vc <- dat.>vertexCount.>rr' >>= fromIntegral # pure

  ip <- Mem.add ip vc
  ip <- Mem.add ip (1 + vc)
  ip <- Mem.add ip (2 + vc)

  dat.>vertexCount.>rm' ! (+3)

  pure ()


addLine2 :: IORef RenderVertFragDataDefault -> Line2 Float -> Float -> Vec3 Float -> IO ()
addLine2 mdat line zLevel color = do
  dat <- readIORef mdat
  let vp = dat.>vertexPool
  let ip = dat.>indexPool
  vc <- dat.>vertexCount.>rr' >>= fromIntegral # pure

  let _start = line.>start
  let _end = line.>end
  (vp.>Mem.add) $ realToFrac (_start.>x)
  (vp.>Mem.add) $ realToFrac (_start.>y)
  (vp.>Mem.add) $ realToFrac zLevel
  (vp.>Mem.add) $ realToFrac (color.>x)
  (vp.>Mem.add) $ realToFrac (color.>y)
  (vp.>Mem.add) $ realToFrac (color.>z)

  (vp.>Mem.add) $ realToFrac (_end.>x)
  (vp.>Mem.add) $ realToFrac (_end.>y)
  (vp.>Mem.add) $ realToFrac zLevel
  (vp.>Mem.add) $ realToFrac (color.>x)
  (vp.>Mem.add) $ realToFrac (color.>y)
  (vp.>Mem.add) $ realToFrac (color.>z)

  ip <- Mem.add ip vc
  ip <- Mem.add ip (1 + vc)
  dat.>vertexCount.>rm' ! (+2)

  pure ()





newDefaultData :: IO (IORef RenderVertFragDataDefault)
newDefaultData = do
  vp <- Mem.new 8 :: IO (Mem.MemBlock CFloat)
  ip <- Mem.new 8 :: IO (Mem.MemBlock CInt)
  false <- newIORef False
  vc <- newIORef (0 :: Int)
  vao <- newIORef (0 :: Int)
  vbo <- newIORef (0 :: Int)
  ebo <- newIORef (0 :: Int)
  let dat = RenderVertFragDataDefault vp ip vc vao vbo ebo false
  newIORef dat

--return data and renderer. CAUTION ! renderer is bind to the data returned (by reference), changing the data changes how rendering occurs
renderVertFragDefault :: IO Int -> IO Int -> IO () -> IO String -> IO (IORef RenderVertFragDataDefault, RenderVertFrag)
renderVertFragDefault mode vertSize setAttribPointers shaderName = do
    mdat <- newDefaultData

    let
      construct :: IORef RenderVertFragDataDefault -> IO Bool
      construct mdat = do
          dat@(RenderVertFragDataDefault vp ip _ _ _ _ _) <- readIORef mdat
          const <- (dat.>constructed).>readIORef
          stored_v <- (vp.>Mem.stored).>readIORef
          stored_i <- (ip.>Mem.stored).>readIORef
          ptr_v <- (vp.>Mem.ptr).>readIORef
          ptr_i <- (ip.>Mem.ptr).>readIORef
          if not const then do

            _vao <- glGenVertexArrays >>= dat.>vao.>rw'
            _vbo <- glGenBuffers >>= dat.>vbo.>rw'
            _ebo <- glGenBuffers >>= dat.>ebo.>rw'


            glBindVertexArray _vao
            glBindBuffer c_GL_ARRAY_BUFFER _vbo
            glBufferData c_GL_ARRAY_BUFFER (4 * stored_v) (castPtr ptr_v) c_GL_STATIC_DRAW

            glBindBuffer c_GL_ELEMENT_ARRAY_BUFFER _ebo
            glBufferData c_GL_ELEMENT_ARRAY_BUFFER (4 * stored_i) (castPtr ptr_i) c_GL_STATIC_DRAW


            setAttribPointers


            glBindBuffer c_GL_ARRAY_BUFFER 0
            glBindVertexArray 0

            dat.>constructed.>rw' ! True
            pure True
          else
            pure False

      deconstruct :: IORef RenderVertFragDataDefault -> IO Bool
      deconstruct mdat = do
        dat@(RenderVertFragDataDefault vp ip vc vao vbo ebo _) <- readIORef mdat
        const <- (dat.>constructed).>readIORef

        if const then do
          vao.>rr' >>= glDeleteVertexArrays
          vbo.>rr' >>= glDeleteBuffers
          ebo.>rr' >>= glDeleteBuffers
          dat.>constructed.>rw' ! False
          pure True
        else
          pure False

      draw :: IORef RenderVertFragDataDefault -> IO Bool
      draw mdat = do
        dat <- readIORef mdat
        const <- dat.>constructed.>readIORef
        stored_i <- dat.>indexPool.>Mem.stored.>readIORef
        if const then do
          dat.>vao.>rr' >>= glBindVertexArray
          smode <- mode
          glDrawElements smode stored_i c_GL_UNSIGNED_INT nullPtr
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
