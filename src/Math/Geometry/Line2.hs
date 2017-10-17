module Math.Geometry.Line2 where


import Math.Linear.Vec
import Common

data Line2 a = Line2{start :: Vec 2 a, end :: Vec 2 a}

instance (UVec a, Show a) => Show (Line2 a) where
  show line = "Line2(start = " ++ show (line.>start) ++ ", end = " ++ show (line.>end) ++ ")"
