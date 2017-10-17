module Math.Geometry.Triangle where


import Math.Linear.Vec


data Triangle a = Triangle{ p1 :: (Vec 3 a) ,
                            p2 :: (Vec 3 a) ,
                            p3 :: (Vec 3 a) }


triangleArea :: (Floating a, AlgVec a) => Triangle a -> a
triangleArea (Triangle p1 p2 p3) = 0.5 * (mag $ cross (p1 |-| p2) (p1 |-| p3))
