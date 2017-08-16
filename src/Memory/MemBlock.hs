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

--MemBlock sizeOfAllocatedMemoryDividedBySizeOfOneElement pointerToAllocatedMemory, actuallyStoredAmountOfElements
data MemBlock a = MemBlock {size :: Int, ptr :: Ptr a, stored :: Int}


new :: (Storable a) => Int -> IO (MemBlock a)
new count = do
  ptr <- mallocArray count :: (Storable a) => IO (Ptr a)
  pure $ MemBlock count ptr 0

delete :: (Storable a) => MemBlock a -> IO ()
delete mem = free $ ptr mem

write :: (Storable a) => MemBlock a -> Int -> a -> IO (MemBlock a)
write mem index val = do
  let p = ptr mem
  let indexed = p `plusPtr` fromIntegral (index * sizeOf val)
  poke indexed val
  pure mem

read :: forall a . (Storable a) => MemBlock a -> Int -> IO a
read mem index = do
  let p = ptr mem
  let indexed = p `plusPtr` fromIntegral (index * sizeOf (undefined :: a))
  peek indexed


grow :: forall a . (Storable a) => MemBlock a -> Int -> IO (MemBlock a)
grow mem@(MemBlock size ptr stored) newSize =
  if newSize <= size then pure mem
    else do
      newPtr <- reallocBytes ptr ( sizeOf(undefined :: a)  * newSize)
      pure $ MemBlock newSize newPtr stored

add :: (Storable a) => MemBlock a -> a -> IO (MemBlock a)
add mem@(MemBlock size ptr actually) element =
  if actually + 1 > size then do
    mem <- grow mem (size * 2) --double the size
    (MemBlock size ptr _) <- write mem actually element
    pure $ MemBlock size ptr $ actually + 1
  else do
    write mem actually element
    pure (MemBlock size ptr $ actually + 1)

foreach :: (Storable a) => MemBlock a -> (a -> IO ()) -> IO ()
foreach mem func = do
  for 0 (< stored mem) (+1) () $ \i _ -> do
    elem <- Memory.MemBlock.read mem i
    func elem
  pure ()
