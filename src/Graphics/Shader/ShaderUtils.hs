module Graphics.Shader.ShaderUtils where

import System.IO
import Graphics.OpenGL
import Foreign.Marshal.Alloc
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.Storable (peek)
import Common
import Math.Nat
import Math.Linear.Mat

data Shader = Shader {id :: Int}

isInUse :: Shader -> IO Bool
isInUse (Shader id) = do
  ptr <- malloc :: IO (Ptr CInt)
  glGetIntegerv c_GL_CURRENT_PROGRAM ptr
  ret <- peek ptr
  pure $ fromIntegral ret == id

enable :: Shader -> IO Bool
enable shader@(Shader id) = do
  using <- isInUse shader
  if using then pure False
  else do glUseProgram id
          pure True

disable :: Shader -> IO Bool
disable shader@(Shader id) = do
  using <- isInUse shader
  if not using then pure False
  else do glUseProgram 0
          pure True

setMat4 :: Shader -> String -> Mat 4 4 Float -> Bool -> IO ()
setMat4 (Shader id) name mat transpose = do
  loc <- glGetUniformLocation id name
  glUniformMatrix4fv loc transpose mat

-- args: pathToVertShader, pathToFragShader
loadShader :: String -> String -> IO Int
loadShader vertPath fragPath = do
    vertStr <- readFile vertPath
    fragStr <- readFile fragPath

    prog <- glCreateProgram
    vertID <- glCreateShader c_GL_VERTEX_SHADER
    fragID <- glCreateShader c_GL_FRAGMENT_SHADER

    glShaderSource vertID 1 vertStr
    glShaderSource fragID 1 fragStr
    glCompileShader vertID

    status <- malloc :: IO (Ptr CInt)
    glGetShaderiv vertID c_GL_COMPILE_STATUS status
    statusGet <- peek status :: IO CInt

    if statusGet == 0 then do
      er <- glGetShaderInfoLog vertID
      error $ "Cannot load vertex shader: " ++ vertPath ++ ", failed with error message : " ++ er
    else do
      free status

      glCompileShader fragID
      status <- malloc :: IO (Ptr CInt)
      glGetShaderiv fragID c_GL_COMPILE_STATUS status
      statusGet <- peek status :: IO CInt

      if statusGet == 0 then do
        er <- glGetShaderInfoLog vertID
        error $ "Cannot load fragment shader: " ++ fragPath ++ ", failed with error message : " ++ er
      else do
        glAttachShader prog vertID
        glAttachShader prog fragID

        glLinkProgram prog
        glValidateProgram prog

        pure prog
