{-#LANGUAGE TypeApplications#-}

module Main where

import Prelude hiding (length)
import qualified Prelude
import Data.Ratio
import Control.Monad
import System.Environment.FindBin
import qualified Data.HashTable.IO as H

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr (Ptr,nullPtr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as UC

import System.FilePath
import System.Directory

import Math.Linear.Mat
import qualified Math.Linear.Mat as Mat
import Math.Nat
import Math.Linear.Vec
import qualified Math.Linear.Vec as Vec
import Timer
import Common
import Math.Geometry.Triangle
import Graphics.OpenGL
import Graphics.Shader.ShaderUtils
import qualified Graphics.Shader.ShaderUtils as SU
import qualified Memory.MemBlock as Mem
import Graphics.Render.RenderVertFrag
import qualified Data.Array.IO as Ar
import Data.IORef
import System.IO.Unsafe
import qualified Defaults as Def
import qualified Render.Renderer as Renderer
import Control.Monad.Primitive
import qualified Registry as Reg
import Registry (Registry(..), Pack(..), PackData(..))
import WindowInfo(WindowInfo(..))
import qualified WindowInfo
import qualified Pack.CorePack as CorePack
import qualified Memory.ArrayBuffer as ArrayBuffer
import Memory.ArrayBuffer (ArrayBuffer)
import Data.Maybe
import Data.Global
import Data.Singletons.TypeLits
import Math.Collision
import Math.Geometry.Line2

import Control.Lens hiding (element, (.>))

name = "Haskell-Voxelized2D"


declareIORef "windowInfo"
  [t|WindowInfo|]
  [e|WindowInfo nullPtr 0 0|] --uninitialized

declareIORef "packData"
  [t|Maybe PackData|]
  [e|Nothing|]

declareIORef "errorCallbackPtr"
  [t|FunPtr GLFWerrorfun|]
  [e|nullFunPtr|]

declareIORef "frameBufferSizeCallbackPtr"
  [t|FunPtr GLFWframebuffersizefun|]
  [e|nullFunPtr|]

declareIORef "keyCallbackPtr"             --TODO don't hold these pointers like that, probably add them to some auto-cleaning facility ;)
  [t|FunPtr GLFWkeyfun|]
  [e|nullFunPtr|]

declareIORef "mouseCallbackPtr"
  [t|FunPtr GLFWmousebuttonfun|]
  [e|nullFunPtr|]

{-packData :: IORef (Maybe PackData) --global reference to fully mutable PackData, should be (Just data) after initialization
{-# NOINLINE packData #-}
packData = unsafePerformIO (newIORef Nothing)

errorCallbackPtr :: IORef (FunPtr GLFWerrorfun)
{-# NOINLINE errorCallbackPtr #-}
errorCallbackPtr = unsafePerformIO (newIORef nullFunPtr)

frameBufferSizeCallbackPtr :: IORef (FunPtr GLFWframebuffersizefun)
{-# NOINLINE frameBufferSizeCallbackPtr #-}
frameBufferSizeCallbackPtr = unsafePerformIO (newIORef nullFunPtr)

keyCallbackPtr :: IORef (FunPtr GLFWkeyfun)
{-# NOINLINE keyCallbackPtr #-}
keyCallbackPtr = unsafePerformIO (newIORef nullFunPtr)

mouseCallbackPtr :: IORef (FunPtr GLFWmousebuttonfun)
{-# NOINLINE mouseCallbackPtr #-}
mouseCallbackPtr = unsafePerformIO (newIORef nullFunPtr)-}

--------------------------------------------------------------------------

shadersDir = "assets" ++ [pathSeparator] ++ "shaders" ++ [pathSeparator]



initGL = do
  glfwOk <- glfwInit

  glfwSetErrorCallback errorCallback

  if glfwOk == c_GLFW_TRUE then do
    println "GLFW3 - done"
    glfwWindowHint c_GLFW_CONTEXT_VERSION_MAJOR 3
    glfwWindowHint c_GLFW_CONTEXT_VERSION_MINOR 3
    glfwWindowHint c_GLFW_OPENGL_PROFILE c_GLFW_OPENGL_CORE_PROFILE
    w <- glfwCreateWindow Def.startingWidth Def.startingHeight name
    glfwMakeContextCurrent w

    gladOk <- gladInit
    if gladOk == c_GLFW_TRUE then do
      println "GLAD - done"


      writeIORef frameBufferSizeCallbackPtr <$> glfwSetFramebufferSizeCallback w frameBufferSizeCallback
      writeIORef keyCallbackPtr <$> glfwSetKeyCallback w keyCallback
      writeIORef mouseCallbackPtr <$> glfwSetMouseButtonCallback w mouseCallback

      let info = WindowInfo w Def.startingWidth Def.startingHeight
      writeIORef windowInfo info
      println "OpenGL initialized"

      println <$> glGetString(c_GL_VERSION)

      return ()
    else
      error "failed to initialize GLAD"
  else
    error "failed to initialize GLFW3"

loadShaders :: IO (HashTable String Shader)
loadShaders = do
  exedir <- getProgPath
  let fullDir = exedir ++ [pathSeparator] ++ shadersDir
  println $ "looking for shaders at " ++ fullDir
  contents <- filter (\name -> Prelude.length name > 2) <$> getDirectoryContents fullDir --sort out `.` and `..`
  mapM_ println contents --print all shaders
  let names = removeDuplicates (dropExtension <$> contents)
  println "importing shaders :"
  mapM_ println names
  let paired = map (\name -> let prefix = fullDir ++ name in (prefix ++ ".vert", prefix ++ ".frag") ) names -- [(fullvert,fullfrag)]

  res <- H.new :: IO (HashTable String Shader)

  let len = Prelude.length names
  _ <- for 0 (<len) (+1) paired $ \ _ ((f,s) : xs) -> do
    let name = dropExtension $ takeFileName f
    i <- loadShader f s
    H.insert res name (Shader i)
    pure xs

  println "Shaders loaded"

  pure res


initRegistry :: IO Registry
initRegistry = do
  packs <- ArrayBuffer.new 8 :: IO (ArrayBuffer Pack)
  keyCallbacks <- ArrayBuffer.new 8 :: IO (ArrayBuffer GLFWkeyfun)
  mouseCallbacks <- ArrayBuffer.new 8 :: IO (ArrayBuffer GLFWmousebuttonfun)
  updateCallbacks <- ArrayBuffer.new 8 :: IO (ArrayBuffer (IORef WindowInfo -> IO () ) )
  let
    addPack :: Pack -> IO ()
    addPack e = do
      ArrayBuffer.push packs e
      pure ()

    addKeyCallback :: GLFWkeyfun -> IO ()
    addKeyCallback fun = do
      ArrayBuffer.push keyCallbacks fun
      pure ()

    addMouseCallback fun = do
      ArrayBuffer.push mouseCallbacks fun
      pure ()

    addUpdateCallback :: (IORef WindowInfo -> IO ()) -> IO ()
    addUpdateCallback fun = do
      ArrayBuffer.push updateCallbacks fun
      pure ()

  n1 <- ArrayBuffer.new 8
  n2 <- ArrayBuffer.new 8
  rw' Reg.lifetimeOneDrawRenderers (Just n1)
  rw' Reg.lifetimeManualRenderers (Just n2)
  
  let reg = Registry addPack addKeyCallback addMouseCallback addUpdateCallback
  let lPackData = PackData packs keyCallbacks mouseCallbacks updateCallbacks
  writeIORef packData $ Just lPackData
  pure reg

sysLoadPacks :: HashTable String Shader -> Registry -> IO ()
sysLoadPacks shaders registry = do
  (Just _packData) <- readIORef packData
  let packs = _packData^.Reg.packs
  cfor 0 (\i -> pure (i <) <*> ArrayBuffer.size packs) (+1) $ \i -> do
    pack <- ArrayBuffer.read packs i
    (view Reg.init pack) registry windowInfo

sysUnloadPacks :: HashTable String Shader -> Registry -> IO ()
sysUnloadPacks shaders registry = do
  (Just _packData) <- readIORef packData
  let packs = _packData^.Reg.packs
  cfor 0 (\i -> pure(i <) <*> ArrayBuffer.size packs) (+1) $ \i -> do
    pack <- ArrayBuffer.read packs i
    (view Reg.deinit pack) registry windowInfo

sysInit :: IO (HashTable String Shader, Registry)
sysInit = do
  initGL
  shaders <- loadShaders
  reg <- initRegistry
  return (shaders,reg)


sysUpdate :: HashTable String Shader -> Registry -> IO ()
sysUpdate shaders registry = do
  (Just _packData) <- readIORef packData
  ArrayBuffer.mapM_ (\fun -> fun windowInfo) (_packData^.Reg.updateCallbacks)

sysRun :: HashTable String Shader -> Registry -> IO ()
sysRun shaders registry = do

  let shouldNotClose w = (== c_GLFW_FALSE) <$> glfwWindowShouldClose w

  _win <- readIORef windowInfo
  while shouldNotClose (_win^.WindowInfo.handle) $ \w -> do

       sysUpdate shaders registry



       Renderer.sysDraw windowInfo shaders


       
       {-glClear c_GL_COLOR_BUFFER_BIT
       glClearColor 0 1 0 1

       let tr =
             Triangle (vec3 (-1) (-1) 0) (vec3 1 0 0) (vec3 0 1 0)
       (dat,ren) <- renderVertFragDefault (pure c_GL_TRIANGLES)
         vertexSizeColor setAttributePointersColor (pure "color")
       addTriangle dat tr (vec3 1 1 0)
       construct ren
       (Just shader) <- H.lookup shaders "color"
       enable shader
       setMat4 shader "P" (identity (SNat @4)) False
       setMat4 shader "V" (identity (SNat @4)) False
       draw ren
       deconstruct ren
       --free ren
       glfwSwapBuffers $ windowId _win -}


       glfwPollEvents

       
       
       return w

  pure ()


sysTerminate shaders = do
  glfwTerminate
  --free all callbacks (on haskell side)
  freeHaskellFunPtr <$> readIORef errorCallbackPtr
  freeHaskellFunPtr <$> readIORef frameBufferSizeCallbackPtr
  freeHaskellFunPtr <$> readIORef keyCallbackPtr
  freeHaskellFunPtr <$> readIORef mouseCallbackPtr

  pure ()

runVoxelized :: IO ()
runVoxelized = do
  (shaders,registry) <- sysInit

  --add core pack
  (view Reg.addPack registry) CorePack.pack

  sysLoadPacks shaders registry

  sysRun shaders registry

  sysUnloadPacks shaders registry

  sysTerminate shaders


main :: IO ()
main = do

  --testVec
  runVoxelized
  




--callbacks
frameBufferSizeCallback :: GLFWframebuffersizefun
frameBufferSizeCallback w width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  writeIORef windowInfo $ WindowInfo w (fromIntegral width) (fromIntegral height)

keyCallback :: GLFWkeyfun
keyCallback w key scancode action mods =

  if fromIntegral key == c_GLFW_KEY_ESCAPE then
    glfwSetWindowShouldClose w True

  else do
    (Just packData) <- readIORef packData
    let callbacks = packData^.Reg.keyCallbacks

    cfor 0 (\i -> pure(i <) <*> ArrayBuffer.size callbacks) (+1) $ \i -> do
      callback <- ArrayBuffer.read callbacks i
      callback w key scancode action mods --call all registred callbacks

    pure ()

mouseCallback :: GLFWmousebuttonfun
mouseCallback w button action mods = do
  (Just packData) <- readIORef packData
  let callbacks = packData^.Reg.mouseCallbacks

  cfor 0 (\i -> pure(i <) <*> ArrayBuffer.size callbacks) (+1) $ \i -> do
    callback <- ArrayBuffer.read callbacks i
    callback w button action mods --call all registred callbacks

  pure ()


errorCallback :: CInt -> CString -> IO ()
errorCallback errcode cstr = do
  str <- peekCString cstr
  println $ "GLFW3 failed with error code : " ++ show errcode ++ ", description " ++ str
---------------------


testFunction :: IO ()
testFunction = do
  let size = 15
  arr <- Ar.newArray (0, size - 1) 0 :: IO (Ar.IOArray Int Int)
  cfor' 0 (< size) (+ 1) $ \i -> Ar.writeArray arr i (size - i)
  facSort arr


  cfor' 0 (< size) (+ 1) $ \ i -> do
    el <- Ar.readArray arr i
    println $ show el

  pure ()
