module Math.Linear.Mat where


import Math.Linear.Vec
import qualified Math.Linear.Vec as Vec
import Math.Nat
import TypeClass.Append

type Mat n m a = Vec n (Vec m a)

mat4 :: Vec N4 a -> Vec N4 a -> Vec N4 a -> Vec N4 a -> Mat N4 N4 a
mat4 a b c d =
  a :> b :> c :> d :> Nil

--                  0 1 2 3 4 ... n-1
--returns Vec n a : 1 0 0 0 0 ... 0
helper :: (Num a) => SNat n -> Vec n a
helper (S n) = 1 :> fullVec n 0
helper Z = Nil

--                  0 1 2 ... m . . . . . . n-1
--returns Vec n a : 0 0 0 ... 1 0 0 0 0 ... 0
helper2 :: (Num a) => SNat n -> SNat m -> Vec n a
--helper2 Z _ = Nil
helper2 size Z = helper size
helper2 (S n) (S m) = 0 :> helper2 n m

identityHelper :: (Num a) => SNat n -> SNat m -> Mat n m a
identityHelper (S Z) n = helper2 n (n %:- n1) :> Nil
identityHelper m@(S q) n = helper2 n (n %:- m) :> identityHelper q n

--return identity matrix of particular size
identity :: (Num a) => SNat n -> Mat n n a
identity n = identityHelper n n

foreach :: (a -> b -> IO b) -> Mat n m a -> b -> IO b
foreach f ( x :> xs ) b = do
   b <- Vec.foreach f x b
   Math.Linear.Mat.foreach f xs b
foreach f _ b = pure b

--column major (feed to OpenGL without tranposing)
ortho :: Float -> Float -> Float -> Float -> Float -> Float -> Mat N4 N4 Float
ortho left right bottom top near far =
  let
    a11 = 2 / (right - left)
    a12 = 0
    a13 = 0
    a14 = -(right + left)/(right - left)
    a21 = 0
    a22 = 2 / (top - bottom)
    a23 = 0
    a24 = -(top + bottom) / (top - bottom)
    a31 = 0
    a32 = 0
    a33 = -2 / (far - near)
    a34 = -(far + near) / (far - near)
    a41 = 0
    a42 = 0
    a43 = 0
    a44 = 1
  in
    mat4 (vec4 a11 a12 a13 a14)
         (vec4 a21 a22 a23 a24)
         (vec4 a31 a32 a33 a34)
         (vec4 a41 a42 a43 a44)
