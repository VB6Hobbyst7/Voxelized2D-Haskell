module Graphics.OpenGL where


--GLFW3 + GLAD bindings using inline-c

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr (Ptr,nullPtr)
import Control.Monad
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as UC
import Math.Linear.Mat
import qualified Math.Linear.Mat as Mat
import qualified Memory.MemBlock as Mem
import Math.Nat

--C.include "<glfw3dll.h>"
C.include "<math.h>"
C.include "<glad.h>"
C.include "<glfw3.h>"
C.include "<stdio.h>"


c_GLFW_TRUE  = 1 :: Int
c_GLFW_FALSE = 0 :: Int
c_GL_COLOR_BUFFER_BIT = 0x00004000 :: Int
c_GL_VERTEX_SHADER = 35633 :: Int
c_GL_FRAGMENT_SHADER = 35632 :: Int
c_GL_COMPILE_STATUS = 35713 :: Int
c_GL_ARRAY_BUFFER = 34962 :: Int
c_GL_ELEMENT_ARRAY_BUFFER = 34963 :: Int
c_GL_STATIC_DRAW = 35044 :: Int
c_GL_FLOAT = 5126 :: Int
c_GL_UNSIGNED_INT = 5125 :: Int
c_GL_TRIANGLES = 4 :: Int
c_GL_LINES = 1 :: Int
c_GL_CURRENT_PROGRAM = 35725 :: Int

glfwInit :: IO Int
glfwInit = fromIntegral <$> [C.exp|int{glfwInit()}|]

glfwCreateWindow :: Int -> Int -> String -> IO (Ptr ())
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
            let !ret = [C.exp|void*{glfwCreateWindow($(int w), $(int h), $(char* n), NULL, NULL)}|]
            free n
            ret

glfwTerminate :: IO ()
glfwTerminate = [C.exp|void{glfwTerminate()}|]

glfwMakeContextCurrent :: Ptr () -> IO ()
glfwMakeContextCurrent handle = do
    [C.exp|void{glfwMakeContextCurrent( $(void* handle) )}|]

glfwWindowShouldClose :: Ptr () -> IO Int
glfwWindowShouldClose handle = do
    fromIntegral <$> [C.exp|int{glfwWindowShouldClose( $(void* handle) )}|]

glGetUniformLocation :: Int -> String -> IO Int
glGetUniformLocation program name = do
  let c1 = fromIntegral program :: CInt
  c2 <- newCString name
  !res <- fromIntegral <$> [C.exp|int{glGetUniformLocation ($(int c1), $(char* c2))}|]
  free c2
  pure res

glUniform1i :: Int -> Int -> IO ()
glUniform1i uniform val = do
  let c1 = fromIntegral uniform :: CInt
  let c2 = fromIntegral val :: CInt
  [C.exp|void{glUniform1i ( $(int c1), $(int c2) ) }|]

glUniform1f :: Int -> Float -> IO ()
glUniform1f uniform val = do
  let c1 = fromIntegral uniform :: CInt
  let c2 = realToFrac val :: CFloat
  [C.exp|void{glUniform1f ( $(int c1), $(float c2) ) }|]

glUniform2f :: Int -> Float -> Float -> IO ()
glUniform2f uniform val1 val2 = do
  let c1 = fromIntegral uniform :: CInt
  let c2 = realToFrac val1 :: CFloat
  let c3 = realToFrac val2 :: CFloat
  [C.exp|void{glUniform2f ( $(int c1), $(float c2), $(float c3) ) }|]

glUniform3f :: Int -> Float -> Float -> Float -> IO ()
glUniform3f uniform val1 val2 val3 = do
  let c1 = fromIntegral uniform :: CInt
  let c2 = realToFrac val1 :: CFloat
  let c3 = realToFrac val2 :: CFloat
  let c4 = realToFrac val3 :: CFloat
  [C.exp|void{glUniform3f ( $(int c1), $(float c2), $(float c3), $(float c4) ) }|]

glUniform4f :: Int -> Float -> Float -> Float -> Float -> IO ()
glUniform4f uniform val1 val2 val3 val4 = do
  let c1 = fromIntegral uniform :: CInt
  let c2 = realToFrac val1 :: CFloat
  let c3 = realToFrac val2 :: CFloat
  let c4 = realToFrac val3 :: CFloat
  let c5 = realToFrac val4 :: CFloat
  [C.exp|void{glUniform4f ( $(int c1), $(float c2), $(float c3), $(float c4) , $(float c5)) }|]

glUniformMatrix4fv :: Int -> Bool -> Mat N4 N4 Float -> IO ()
glUniformMatrix4fv uniform transpose mat = do
  let c1 = fromIntegral uniform :: CInt
  let c2 = if transpose then 1 else 0 :: CChar
  mem <- Mem.new 16 :: IO (Mem.MemBlock CFloat)
  mem@(Mem.MemBlock _ ptr _) <- Mat.foreach (\ v mem -> Mem.add mem $ realToFrac v) mat mem
  !_ <- [C.exp|void{glUniformMatrix4fv ( $(int c1), 1, $(char c2),  $(float* ptr)) }|]
  Mem.delete mem

glGetIntegerv :: Int -> Ptr CInt -> IO ()
glGetIntegerv par out = do
  let c1 = fromIntegral par :: CInt
  [C.exp|void{glGetIntegerv ( $(int c1), $(int* out) )}|]

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


glViewport :: Int -> Int -> Int -> Int -> IO ()
glViewport x y width height = do
  let c1 = fromIntegral x :: CInt
  let c2 = fromIntegral y :: CInt
  let c3 = fromIntegral width :: CInt
  let c4 = fromIntegral height :: CInt

  [C.exp|void{glViewport ($(int c1), $(int c2), $(int c3), $(int c4)) }|]

glCreateProgram :: IO Int
glCreateProgram =
    fromIntegral <$> [C.exp|int{glCreateProgram()}|]

glCreateShader :: Int -> IO Int
glCreateShader mode =
    let f = fromIntegral mode :: CInt
    in  fromIntegral <$> [C.exp|int{glCreateShader( $(int f) )}|]

glShaderSource :: Int -> Int -> String -> IO ()
glShaderSource handle count source = do
    let h = fromIntegral handle :: CInt
    let c = fromIntegral count :: CInt
    str <- newCString source
    _ <- [C.exp|void{glShaderSource( $(int h), $(int c), & $(char* str), NULL)}|]
    free str
    pure ()

glCompileShader :: Int -> IO ()
glCompileShader handle = do
    let h = fromIntegral handle :: CInt
    [C.exp|void{glCompileShader( $(int h) )}|]

glGetShaderiv :: Int -> Int -> Ptr CInt -> IO ()
glGetShaderiv shader mode result = do
    let s = fromIntegral shader :: CInt
    let m = fromIntegral mode :: CInt
    [C.exp|void{glGetShaderiv( $(int s), $(int m), $(int* result))}|]


glGenVertexArrays :: IO Int
glGenVertexArrays = do
    val <- malloc :: IO (Ptr CInt)
    [C.exp|void{glGenVertexArrays( 1, $(int* val) )}|]
    !result <- peek val :: IO CInt
    free val
    pure $ fromIntegral result

glGenBuffers :: IO Int
glGenBuffers = do
    val <- malloc :: IO (Ptr CInt)
    [C.exp|void{glGenBuffers( 1, $(int* val) )}|]
    !result <- peek val :: IO CInt
    free val
    pure $ fromIntegral result

glBindVertexArray :: Int -> IO ()
glBindVertexArray val = do
    let con = fromIntegral val :: CInt
    [C.exp|void{glBindVertexArray( $(int con) )}|]

glBindBuffer :: Int -> Int -> IO ()
glBindBuffer target buffer = do
    let con1 = fromIntegral target :: CInt
    let con2 = fromIntegral buffer :: CInt
    [C.exp|void{glBindBuffer( $(int con1) , $(int con2))}|]

glBufferData :: Int -> Int -> Ptr () -> Int -> IO ()
glBufferData buffer size dat usage = do
  let con1 = fromIntegral buffer :: CInt
  let con2 = fromIntegral size :: CInt
  let con3 = fromIntegral usage :: CInt
  [C.exp|void{glBufferData( $(int con1) , $(int con2), $(void* dat), $(int con3))}|]

glDeleteVertexArrays :: Int -> IO ()
glDeleteVertexArrays array = do
    let con1 = fromIntegral array :: CInt
    [C.exp|void{glDeleteVertexArrays(1, & $(int con1))}|]

glDeleteBuffers :: Int -> IO ()
glDeleteBuffers buffer = do
    let con1 = fromIntegral buffer :: CInt
    [C.exp|void{glDeleteBuffers(1, & $(int con1))}|]

glDrawElements :: Int -> Int -> Int -> Ptr () -> IO ()
glDrawElements mode count typee indices = do
    let con1 = fromIntegral mode :: CInt
    let con2 = fromIntegral count :: CInt
    let con3 = fromIntegral typee :: CInt
    [C.exp|void{glDrawElements( $(int con1), $(int con2), $(int con3), $(void* indices))}|]

glVertexAttribPointer :: Int -> Int -> Int -> Bool -> Int -> Integer -> IO ()
glVertexAttribPointer index size typee normalized stride pointer = do
    let con1 = fromIntegral index :: CInt
    let con2 = fromIntegral size :: CInt
    let con3 = fromIntegral typee :: CInt
    let con4 = if normalized then 1 else 0 :: CChar
    let con5 = fromIntegral stride :: CInt
    let con6 = fromIntegral pointer :: CLong
    [C.exp|void{glVertexAttribPointer( $(int con1), $(int con2), $(int con3), $(char con4), $(int con5), $(long con6))}|]

glEnableVertexAttribArray :: Int -> IO ()
glEnableVertexAttribArray array = do
    let con1 = fromIntegral array :: CInt
    [C.exp|void{glEnableVertexAttribArray($(int con1))}|]

glGetShaderInfoLog :: Int -> IO String
glGetShaderInfoLog shader = do
    let s = fromIntegral shader :: CInt
    let c = 512 :: CInt
    let allocated = (mallocBytes $ fromIntegral c) :: IO CString
    str <- allocated
    [C.exp|void{glGetShaderInfoLog( $(int s), $(int c), NULL, $(char* str))}|]
    let !res = peekCString str
    free str
    res



glAttachShader :: Int -> Int -> IO ()
glAttachShader program shader = do
    let p = fromIntegral program :: CInt
    let s = fromIntegral shader :: CInt
    [C.exp|void{glAttachShader( $(int p), $(int s))}|]

glLinkProgram :: Int -> IO ()
glLinkProgram program = do
    let p = fromIntegral program :: CInt
    [C.exp|void{glLinkProgram( $(int p) )}|]

glValidateProgram :: Int -> IO ()
glValidateProgram program = do
    let p = fromIntegral program :: CInt
    [C.exp|void{glValidateProgram( $(int p))}|]

glUseProgram :: Int -> IO ()
glUseProgram prog = do
    let p = fromIntegral prog :: CInt
    [C.exp|void{glUseProgram( $(int p) )}|]

glfwSwapBuffers :: Ptr () -> IO ()
glfwSwapBuffers handle = do
    [C.exp|void{glfwSwapBuffers(  $(void* handle) )}|]

glfwPollEvents :: IO ()
glfwPollEvents = [C.exp|void{glfwPollEvents()}|]

gladInit :: IO Int
gladInit = fromIntegral <$> [C.exp|int{gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)}|]
