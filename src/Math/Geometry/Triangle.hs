module Math.Geometry.Triangle where

import TypeClass.Vector
import Math.Linear.Vec
import Math.Nat

data Triangle a = Triangle{ p1 :: (Vec N3 a) , p2 :: (Vec N3 a) , p3 :: (Vec N3 a) }


triangleArea :: (Floating a, Vector (Vec N3 a)) => Triangle a -> a
triangleArea (Triangle p1 p2 p3) = 0.5 * (mag $ cross (p1 |-| p2) (p1 |-| p3))
