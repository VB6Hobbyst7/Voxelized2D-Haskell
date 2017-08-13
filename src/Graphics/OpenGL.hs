module Graphics.OpenGL where


--GLFW3 + GLAD bindings using inline-c

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr (Ptr,nullPtr)
import Control.Monad
import qualified Language.C.Inline as C

--C.include "<glfw3dll.h>"
C.include "<math.h>"
C.include "<glad.h>"
C.include "<glfw3.h>"
C.include "<stdio.h>"


c_GLFW_TRUE  = 1 :: Int
c_GLFW_FALSE = 0 :: Int
c_GL_COLOR_BUFFER_BIT = 0x00004000 :: Int

glfwInit :: IO Int
glfwInit = fromIntegral <$> [C.exp|int{glfwInit()}|]
glfwCreateWindow :: Int -> Int -> String -> IO Integer
glfwCreateWindow width height name = let
         w :: CInt
         w = fromIntegral width

         h :: CInt
         h = fromIntegral height

         ion :: IO CString
         ion = newCString name
      in
         do
            n <- ion
            let ret = fromIntegral <$> [C.exp|long{glfwCreateWindow($(int w), $(int h), $(char* n), NULL, NULL)}|]
            free n
            ret

glfwTerminate :: IO ()
glfwTerminate = [C.exp|void{glfwTerminate()}|]

glfwMakeContextCurrent :: Integer -> IO ()
glfwMakeContextCurrent handle = do
    let h = (fromIntegral handle) :: CLong
    [C.exp|void{glfwMakeContextCurrent( $(long h) )}|]

glfwWindowShouldClose :: Integer -> IO Int
glfwWindowShouldClose handle = do
    let h = (fromIntegral handle) :: CLong
    fromIntegral <$> [C.exp|int{glfwWindowShouldClose( $(long h) )}|]

glClear :: Int -> IO ()
glClear bitField = do
    let f = (fromIntegral bitField) :: CUInt
    [C.exp|void{glClear( $(unsigned int f) )}|]

glClearColor :: Float -> Float -> Float -> Float -> IO ()
glClearColor r g b a =
    let f1 = realToFrac r
        f2 = realToFrac g
        f3 = realToFrac b
        f4 = realToFrac a
    in [C.exp|void{glClearColor( $(float f1), $(float f2), $(float f3), $(float f4) )}|]

glfwSwapBuffers :: Integer -> IO ()
glfwSwapBuffers handle = do
    let h = (fromIntegral handle) :: CLong
    [C.exp|void{glfwSwapBuffers( $(long h) )}|]

glfwPollEvents :: IO ()
glfwPollEvents = [C.exp|void{glfwPollEvents()}|]

gladInit :: IO Int
gladInit = fromIntegral <$> [C.exp|int{gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)}|]