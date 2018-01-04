{-# LANGUAGE ScopedTypeVariables #-}

module Memory.MemBlock where



import Foreign
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr (Ptr,nullPtr)
import Control.Monad
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as UC
import Common
import Data.IORef

--MemBlock sizeOfAllocatedMemoryDividedBySizeOfOneElement pointerToAllocatedMemory, actuallyStoredAmountOfElements
data MemBlock a = MemBlock {size :: IORef Int, ptr :: IORef (Ptr a), stored :: IORef Int}


new :: (Storable a) => Int -> IO (MemBlock a)
new count = do
  ptr <- mallocArray count :: (Storable a) => IO (Ptr a)
  wrapped <- rn' ptr
  _count <- rn' count
  zero <- rn' (0 :: Int)
  pure $ MemBlock _count wrapped zero

delete :: (Storable a) => MemBlock a -> IO ()
delete mem = do
  ptr <- mem.>ptr.>rr'
  free ptr

write :: (Storable a) => MemBlock a -> Int -> a -> IO (MemBlock a)
write mem index val = do
  p <- mem.>ptr.>rr'
  let indexed = p `plusPtr` fromIntegral (index * sizeOf val)
  poke indexed val
  pure mem

read :: forall a . (Storable a) => MemBlock a -> Int -> IO a
read mem index = do
  p <- mem.>ptr.>rr'
  let indexed = p `plusPtr` fromIntegral (index * sizeOf (undefined :: a))
  peek indexed


grow :: forall a . (Storable a) => MemBlock a -> Int -> IO (MemBlock a)
grow mem@(MemBlock msize mptr stored) newSize = do
  size <- rr' msize
  ptr <- rr' mptr
  if newSize <= size then
    pure mem
  else do
    newPtr <- reallocBytes ptr ( sizeOf(undefined :: a)  * newSize)

    mptr.>rw' $ newPtr
    msize.>rw' $ newSize

    pure mem

add :: (Storable a) => MemBlock a -> a -> IO (MemBlock a)
add mem@(MemBlock msize mptr mactually) element = do
  size <- rr' msize
  ptr <- rr' mptr
  actually <- rr' mactually
  if actually + 1 > size then do
    grow mem (size * 2) --double the size
    pure ()
  else
    pure ()

  (mem.>write) actually element
  mactually.>rm' $ (+1)
  pure mem

foreach :: (Storable a) => MemBlock a -> (a -> IO ()) -> IO ()
foreach mem func = do
  _stored <- mem.>stored.>rr'
  for 0 (< _stored) (+1) () $ \i _ -> do
    elem <- mem.>Memory.MemBlock.read $ i
    func elem
  pure ()
