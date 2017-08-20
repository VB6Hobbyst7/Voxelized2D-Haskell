

module Registry where

import qualified Data.Vector.Mutable as Vector
import Foreign.Ptr
import Memory.ArrayBuffer(ArrayBuffer(..))
import Graphics.OpenGL(GLFWkeyfun, GLFWmousebuttonfun)

data WindowInfo = WindowInfo{windowId :: Ptr (), windowWidth :: Int, windowHeight :: Int}

data Pack = Pack {packName :: String, packVersion :: String, packInit :: Registry -> IO (), packDeinit :: Registry -> IO ()}

data PackData = PackData{dataPacks :: ArrayBuffer Int Pack, dataKeyCallbacks :: ArrayBuffer Int GLFWkeyfun, dataMouseCallbacks :: ArrayBuffer Int GLFWmousebuttonfun}

data Registry = Registry {
  addPack :: Pack -> IO (),
  addKeyCallback :: GLFWkeyfun -> IO (),
  addMouseCallback :: GLFWmousebuttonfun -> IO ()

}
