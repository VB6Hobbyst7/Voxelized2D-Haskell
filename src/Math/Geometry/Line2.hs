module Math.Geometry.Line2 where

import Math.Nat
import Math.Linear.Vec
import Common

data Line2 a = Line2{start :: Vec N2 a, end :: Vec N2 a}

instance (Show a) => Show (Line2 a) where
  show line = "Line2(start = " ++ show (line.>start) ++ ", end = " ++ show (line.>end) ++ ")"