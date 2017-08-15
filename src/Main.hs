module Main where


import Data.Ratio
import Control.Monad
import Control.Monad.State
import System.Environment.FindBin
import qualified Data.HashMap as HashMap

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


name = "Haskell-voxelized2D"
width = 400 :: Int
height = 400 :: Int

shadersDir = "resources" ++ [pathSeparator] ++ "shaders" ++ [pathSeparator]

initGL = do
  glfwOk <- glfwInit

  if glfwOk == c_GLFW_TRUE then do
    println "GLFW3 - done"
    w <- glfwCreateWindow width height name
    glfwMakeContextCurrent w

    gladOk <- gladInit
    if gladOk == c_GLFW_TRUE then do
      println "GLAD - done"
      return w
    else
      error "failed to initialize GLAD"
  else
    error "failed to initialize GLFW3"

loadShaders :: IO (HashMap.Map String Shader )
loadShaders = do
  exedir <- getProgPath
  let fullDir = exedir ++ [pathSeparator] ++ shadersDir
  println $ "looking for shaders at " ++ fullDir
  contents <- filter (\name -> length name > 2) <$> getDirectoryContents fullDir --sort out `.` and `..`
  mapM_ println contents --TODO --print all shaders
  let names = removeDuplicates (dropExtension <$> contents)
  println "importing shaders :"
  mapM_ println names
  let paired = map (\name -> let prefix = fullDir ++ name in (prefix ++ ".vert", prefix ++ ".frag") ) names -- [(fullvert,fullfrag)]

  let emptyMap = HashMap.empty :: HashMap.Map String Shader

  let len = length names
  (_, res) <- for 0 (<len) (+1) (paired, emptyMap) $ \ _ (  (f,s) : xs   , m ) -> do
    let name = dropExtension $ takeFileName f
    i <- loadShader f s
    pure (xs, HashMap.insert name (Shader i) m)

  mapM_ (println . show) $ HashMap.keys res

  pure res

main :: IO ()
main = do
    !w <- initGL
    shaders <- loadShaders

    --glViewport 0 0 400 400

    let shader = (HashMap.!)  shaders "color"
    let i = SU.id shader
    glUseProgram i

    idP <- glGetUniformLocation i "P"
    idV <- glGetUniformLocation i "V"

    println $ "loc " ++ show idP
    println $ "loc " ++ show idV

    setMat4 idP (identity n4) False
    setMat4 idV (identity n4) False


    (dat, render) <- renderVertFragDefault c_GL_TRIANGLES vertexSizeColor setAttributePointersColor "color"

    dat <- addTriangle dat (Triangle ( vec3 (-1) (-1) 0 ) ( vec3 1 (-1) 0 ) (vec3 0 1 0) )    (vec3 1 1 1)

    (constr, dat) <- construct render dat

    println $ show (Mem.stored $ vertexPool dat)
    println $ show (Mem.stored $ indexPool dat)
    println $ show constr



    let shouldNotClose w = (== c_GLFW_FALSE) <$> glfwWindowShouldClose w

    while shouldNotClose w $ \_ -> do
         glClear c_GL_COLOR_BUFFER_BIT
         glClearColor 1 0 0 1

         draw render dat

         glfwSwapBuffers w
         glfwPollEvents
         return w


    glfwTerminate
