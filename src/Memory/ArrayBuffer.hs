{-# LANGUAGE MultiParamTypeClasses, RankNTypes, FlexibleInstances, InstanceSigs, Strict #-}

module Memory.ArrayBuffer where

import Data.Array.IO
import Data.IORef
import Common

_ArrayBufferDefaultSize = 8 :: Int

--fully mutable record, c++ like (feature lacking) vector
data ArrayBuffer e = ArrayBuffer{marray :: IORef (IOArray Int e), msize :: IORef Int}

grow :: ArrayBuffer e -> Int -> IO (ArrayBuffer e)
grow buf delta = do
  allocSize <- allocatedSize buf
  let newSize = allocSize + delta
  ar <- newArray_ (0, newSize) :: IO (IOArray Int a)
  cfor 0 (\i -> pure (i <) <*> size buf) (+1) $ \i -> do
    t <- Memory.ArrayBuffer.read buf i
    writeArray ar i t
    pure ()
  writeIORef (marray buf) ar
  pure buf

growIfNecessary :: ArrayBuffer e -> Int -> IO ()
growIfNecessary buf newSize = do
  allocSize <- allocatedSize buf
  if allocSize < newSize then do
    grow buf (newSize * 2)                           --allocated size is doubled if full
    pure ()
  else
    pure ()


mapM_ :: (e -> IO a) -> ArrayBuffer e -> IO ()
mapM_ fun buf = do
  len <- buf.>size

  cfor' 0 (< len) (+1) $ \i -> do
    element <- Memory.ArrayBuffer.read buf i
    fun element


--does not free up the elements (gc wont do it), only set the size to 0
clear :: ArrayBuffer e -> IO ()
clear buf = writeIORef (buf.>msize) 0


allocatedSize :: ArrayBuffer e -> IO Int
allocatedSize buf = snd <$> (array buf >>= getBounds)

size :: ArrayBuffer e -> IO Int
size buf = readIORef (msize buf)

array :: ArrayBuffer e -> IO (IOArray Int e)
array buf = readIORef (marray buf)

new :: Int -> IO (ArrayBuffer e)
new size = do
    ar <- newArray_ (0, size) :: IO (IOArray Int a)
    zero <- newIORef 0
    array <- newIORef ar
    pure $ ArrayBuffer array zero

push :: ArrayBuffer e -> e -> IO (ArrayBuffer e)
push buffer el = do
    alloc <- allocatedSize buffer
    size <- size buffer

    growIfNecessary buffer (size + 1)

    ar <- array buffer
    writeArray ar size el
    writeIORef (msize buffer) (size+1)
    pure buffer

read :: ArrayBuffer e -> Int -> IO e
read buf index = do
    ar <- array buf
    readArray ar index

write :: ArrayBuffer e -> Int -> e -> IO (ArrayBuffer e)
write buf i el = do
    ar <- array buf
    writeArray ar i el
    pure buf