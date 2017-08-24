module Math.Geometry.Line2 where

import Math.Nat
import Math.Linear.Vec

data Line2 a = Line2{start :: Vec N2 a, end :: Vec N2 a}
