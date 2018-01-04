module WindowInfo where

import Foreign.Ptr
import Control.Lens hiding (element)

data WindowInfo = WindowInfo{_handle :: Ptr (), _width :: Int, _height :: Int}

makeLenses ''WindowInfo

