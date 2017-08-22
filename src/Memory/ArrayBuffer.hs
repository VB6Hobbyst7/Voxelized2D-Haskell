{-# LANGUAGE MultiParamTypeClasses, RankNTypes, FlexibleInstances, InstanceSigs, Strict #-}

module Memory.ArrayBuffer where

import Data.Array.IO
import Data.IORef
import Common

_arrayBufferDefaultSize = 8 :: Int

class (Num i) => DynamicArrayContainer a i e where --TODO convert back to regular functions
  size :: a i e -> IO i
  allocatedSize :: a i e -> IO i
  array :: a i e -> IO (IOArray i e)
  new :: i -> IO (a i e)
  push :: a i e -> e -> IO (a i e)
  read :: a i e -> i -> IO e
  write :: a i e -> i -> e -> IO (a i e)

--fully mutable record, c++ like (feature lacking) vector
data ArrayBuffer i e = ArrayBuffer{marray :: IORef (IOArray i e), msize :: IORef i}

grow :: ArrayBuffer Int e -> Int -> IO (ArrayBuffer Int e)
grow buf delta = do
  allocSize <- allocatedSize buf
  let newSize = allocSize + delta
  ar <- newArray_ (0, newSize) :: IO (IOArray Int a)
  cfor 0 ( (>) <$> size buf) (+1) $ \i -> do
    t <- Memory.ArrayBuffer.read buf i
    writeArray ar i t
    pure ()
  writeIORef (marray buf) ar
  pure buf

growIfNecessary :: ArrayBuffer Int e -> Int -> IO ()
growIfNecessary buf newSize = do
  allocSize <- allocatedSize buf
  if allocSize < newSize then do
    grow buf (newSize * 2)                           --allocated size is doubled if full
    pure ()
  else
    pure ()


mapM_ :: (e -> IO a) -> ArrayBuffer Int e -> IO ()
mapM_ fun buf = do
  len <- buf.>size

  cfor 0 ((>) <$> buf.>size) (+1) $ \i -> do
    element <- Memory.ArrayBuffer.read buf i
    fun element


--does not free up the elements (gc wont do it), only set the size to 0
clear :: (Num i) => ArrayBuffer i e -> IO ()
clear buf = writeIORef (buf.>msize) 0


instance DynamicArrayContainer ArrayBuffer Int a where
  allocatedSize buf = snd <$> (array buf >>= getBounds)

  size buf = readIORef (msize buf)

  array buf = readIORef (marray buf)


  new size = do
    ar <- newArray_ (0, size) :: IO (IOArray Int a)
    zero <- newIORef 0
    array <- newIORef ar
    pure $ ArrayBuffer array zero

  push buffer el = do
    alloc <- allocatedSize buffer
    size <- size buffer

    growIfNecessary buffer (size + 1)

    ar <- array buffer
    writeArray ar size el
    writeIORef (msize buffer) (size+1)
    pure buffer

  read buf index = do
    ar <- array buf
    readArray ar index

  write buf i el = do
    ar <- array buf
    writeArray ar i el
    pure buf
