module Math.Geometry.Triangle2 where

import Math.Linear.Vec

data Triangle2 a = Triangle2{p1 :: Vec2 a, p2 :: Vec2 a, p3 :: Vec2 a}