module Math.Linear.Mat where


import Math.Linear.Vec
import qualified Math.Linear.Vec as Vec
import Math.Nat
import TypeClass.Append

type Mat n m a = Vec n (Vec m a)

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
