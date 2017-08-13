module Math.Geometry.Triangle where

import TypeClass.Vector
import Math.Linear.Vect
import Math.Nat

data Triangle a = Triangle{ p1 :: (Vect N3 a) , p2 :: (Vect N3 a) , p3 :: (Vect N3 a) }


triangleArea :: (Floating a, Vector (Vect N3 a)) => Triangle a -> a
triangleArea (Triangle p1 p2 p3) = 0.5 * (mag $ cross (p1 |-| p2) (p1 |-| p3))