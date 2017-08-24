module Math.Collision where

import Math.Nat
import Math.Linear.Vec
import Math.Geometry.Line2
import Common
import TypeClass.Vector

distancePoint2Line2 :: (Floating a) => Vec N2 a -> Line2 a -> a
distancePoint2Line2 point line =
  let d = line.>start |-| line.>end
      n = d.>ortho
      vec = point |-| line.>start
  in
      abs $ (n `dot` vec) / n.>mag
