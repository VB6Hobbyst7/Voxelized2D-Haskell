module Main where


import Data.Ratio
import Control.Monad



import Math.Nat
import TypeClass.Vector
import Math.Linear.Vect
import Timer
import Common
import Math.Geometry.Triangle
import Graphics.OpenGL





while :: (a -> IO Bool) -> a -> (a -> IO a) -> IO a
while praed x funktion = do
    ok <- praed x
    if ok
        then
            do
                 y <- funktion x
                 while praed y funktion
        else
            return x


shouldNotClose :: Integer -> IO Bool
shouldNotClose w = do
    s <- glfwWindowShouldClose w
    pure $ s == c_GLFW_FALSE


main :: IO ()
main = do

    let a = (1 :: Double) :> 2 :> 3 :> Nil
    let d = a `dot` a
    let b = a |+| a
    let o = a |-| a

    println $ show d
    println $ show b
    println $ show o

    let triangle = Triangle ( (-1) :> 0 :> 0 :> Nil ) ( 1 :> 0 :> 0 :> Nil ) ( 0 :> 1 :> 0 :> Nil ) :: Triangle Double

    println $ "area of triangle is " ++ show (triangleArea triangle)

    glfwOk <- glfwInit

    println $ if glfwOk == c_GLFW_TRUE then "glfw3 inited" else "init failed"


    w <- glfwCreateWindow 400 400 "cool window"
    glfwMakeContextCurrent w

    gladOk <- gladInit
    println $ if gladOk == c_GLFW_TRUE then "glad inited" else "glad failed"


    while shouldNotClose w $ \ _ -> do
         glClear c_GL_COLOR_BUFFER_BIT
         glClearColor 1 0 0 1
         glfwSwapBuffers w
         glfwPollEvents
         return w


    glfwTerminate
