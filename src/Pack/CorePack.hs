{-# LANGUAGE Strict #-}

module Pack.CorePack where

import Prelude hiding (init)
import Graphics.OpenGL

import qualified Registry
import Common


name = "CorePack"
version = "0.0.1"

keyCallback :: GLFWkeyfun
keyCallback w key scancode action mods = do
  println $ "you pressed the key with number " ++ show key

mouseCallback :: GLFWmousebuttonfun
mouseCallback w button action mods = do
  println $ "you pressed mouse button with number " ++ show button

init reg = do

  Registry.addKeyCallback reg keyCallback
  Registry.addMouseCallback reg mouseCallback


  --error "fuck, man !" --TODO test error
  println "core pack init !"

deinit reg = do
  println "core pack deinit !"

pack = Registry.Pack name version init deinit
