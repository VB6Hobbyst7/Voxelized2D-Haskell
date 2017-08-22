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
import TypeClass.Vector
import Math.Linear.Vec
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
import Registry (Registry(..), Pack(..), WindowInfo(..), PackData(..))
import qualified Pack.CorePack as CorePack
import qualified Memory.ArrayBuffer as ArrayBuffer
import Memory.ArrayBuffer (ArrayBuffer)
import Data.Maybe

name = "Haskell-Voxelized2D"


--global IO variables, worst thing ever. No more
windowInfo :: IORef WindowInfo
{-# NOINLINE windowInfo #-}
windowInfo = unsafePerformIO (newIORef $ WindowInfo nullPtr 0 0) --uninitialized

packData :: IORef (Maybe PackData) --global reference to fully mutable PackData, should be (Just data) after initialization
{-# NOINLINE packData #-}
packData = unsafePerformIO (newIORef Nothing)

errorCallbackPtr :: IORef (FunPtr GLFWerrorfun) --TODO don't hold these pointers like that, probably add them to some auto-cleaning facility ;)
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
mouseCallbackPtr = unsafePerformIO (newIORef nullFunPtr)

--------------------------------------------------------------------------

shadersDir = "resources" ++ [pathSeparator] ++ "shaders" ++ [pathSeparator]



initGL = do
  glfwOk <- glfwInit

  glfwSetErrorCallback errorCallback

  if glfwOk == c_GLFW_TRUE then do
    println "GLFW3 - done"
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

      return info
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
    let name = f |> takeFileName |> dropExtension
    i <- loadShader f s
    H.insert res name (Shader i)
    pure xs

  println "Shaders loaded"

  pure res


initRegistry :: WindowInfo -> IO (Registry,PackData)
initRegistry win = do
  packs <- ArrayBuffer.new 8 :: IO (ArrayBuffer Int Pack)
  keyCallbacks <- ArrayBuffer.new 8 :: IO (ArrayBuffer Int GLFWkeyfun)
  mouseCallbacks <- ArrayBuffer.new 8 :: IO (ArrayBuffer Int GLFWmousebuttonfun)
  let
    addPack :: Pack -> IO ()
    addPack e = do
      ArrayBuffer.push packs e
      pure ()
  let
    addKeyCallback :: GLFWkeyfun -> IO ()
    addKeyCallback fun = do
      ArrayBuffer.push keyCallbacks fun
      pure ()
  let
    addMouseCallback fun = do
      ArrayBuffer.push mouseCallbacks fun
      pure ()
  let reg = Registry addPack addKeyCallback addMouseCallback
  let lPackData = PackData packs keyCallbacks mouseCallbacks
  writeIORef packData $ Just lPackData
  pure (reg,lPackData)

sysLoadPacks :: WindowInfo -> HashTable String Shader -> Registry -> PackData -> IO ()
sysLoadPacks windowInfo shaders registry packData = do
  let packs = Reg.dataPacks packData
  cfor 0 ((>) <$> ArrayBuffer.size packs) (+1) $ \i -> do
    pack <- ArrayBuffer.get packs i
    Reg.packInit pack registry

sysUnloadPacks :: WindowInfo -> HashTable String Shader -> Registry -> PackData -> IO ()
sysUnloadPacks windowInfo shaders registry packData = do
  let packs = Reg.dataPacks packData
  cfor 0 ((>) <$> ArrayBuffer.size packs) (+1) $ \i -> do
    pack <- ArrayBuffer.get packs i
    Reg.packDeinit pack registry

sysInit :: IO (WindowInfo, HashTable String Shader, Registry, PackData)
sysInit = do
  windowInfo <- initGL
  shaders <- loadShaders
  (reg,packData) <- initRegistry windowInfo
  return (windowInfo, shaders,reg,packData)

sysRun windowInfo shaders = do

  let shouldNotClose w = (== c_GLFW_FALSE) <$> glfwWindowShouldClose w

  while shouldNotClose (windowInfo |> Reg.windowId) $ \w -> do

       Renderer.sysDraw windowInfo shaders

       glfwPollEvents
       return w

  pure ()


sysTerminate windowInfo shaders = do
  glfwTerminate
  --free all callbacks (on haskell side)
  freeHaskellFunPtr <$> readIORef errorCallbackPtr
  freeHaskellFunPtr <$> readIORef frameBufferSizeCallbackPtr
  freeHaskellFunPtr <$> readIORef keyCallbackPtr
  freeHaskellFunPtr <$> readIORef mouseCallbackPtr

  pure ()

main :: IO ()
main = do
  (winInfo,shaders,registry,packData) <- sysInit

  --add core pack
  Reg.addPack registry CorePack.pack

  sysLoadPacks winInfo shaders registry packData

  sysRun winInfo shaders

  sysUnloadPacks winInfo shaders registry packData

  sysTerminate winInfo shaders


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
    let callbacks = dataKeyCallbacks packData

    cfor (0 :: Int) ( (>) <$> ArrayBuffer.size callbacks) (+1) $ \i -> do
      callback <- ArrayBuffer.get callbacks i
      callback w key scancode action mods --call all registred callbacks

    pure ()

mouseCallback :: GLFWmousebuttonfun
mouseCallback w button action mods = do
  (Just packData) <- readIORef packData
  let callbacks = dataMouseCallbacks packData

  cfor (0 :: Int) ( (>) <$> ArrayBuffer.size callbacks) (+1) $ \i -> do
    callback <- ArrayBuffer.get callbacks i
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
  cfor 0 ((>) <$> pure size) (+ 1) $ \i -> Ar.writeArray arr i (size - i)
  facSort arr


  cfor 0 ((>) <$> pure size) (+ 1) $ \ i -> do
    el <- Ar.readArray arr i
    println $ show el

  pure ()
