{-Language PolyKinds -}
module Math.Linear.Vec where

import Math.Nat
import TypeClass.Vector
import TypeClass.Append

infixr 8 :>
data  Vec :: Nat -> * -> * where   --same as : data List (n :: Nat) a where ...
  Nil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a

fullVec :: SNat n -> a -> Vec n a
fullVec (S n) val = val :> fullVec n val
fullVec Z _ = Nil


foreach :: (a -> b -> IO b) -> Vec n a -> b -> IO b
foreach f (x :> xs) b = do
  b <- f x b
  foreach f xs b
foreach f _ b = pure b

--TODO make x,y,z functions typechecked
x :: Vec n m -> m
x (first :> _) = first

y :: Vec n m -> m
y (_ :> second :> _) = second

z :: Vec n m -> m
z (_ :> _ :> third :> _) = third

vec2 :: a -> a -> Vec N2 a
vec2 a b = a :> b :> Nil

vec3 :: a -> a -> a -> Vec N3 a
vec3 a b c = a :> b :> c :> Nil

vec4 :: a -> a -> a -> a -> Vec N4 a
vec4 a b c d = a :> b :> c :> d :> Nil

instance (Show a) => Show (Vec n a) where

    show all@(x :> _) = "[" ++ show1 all ++ "]"
      where
        show1 :: (Show a) => Vec n a -> String
        show1 (x :> y :> zs) = show x ++ ", " ++ show1 (y :> zs)
        show1 (x :> Nil) = show x
    show Nil = "[]"

instance (Floating a) => Vector (Vec n a) where
    type T (Vec n a) = a

    (x :> xs) |+| (y :> ys) = (x + y) :> (xs |+| ys)
    Nil |+| ys = ys

    (x :> xs) |-| (y :> ys) = (x - y) :> (xs |-| ys)
    Nil |-| ys = ys

    (x :> xs) `dot` (y :> ys) = (x * y) + (dot xs ys)
    Nil `dot` Nil = 0

    (x :> xs) |* k = (x * k) :> (xs |* k)
    k *| (y :> ys) = (k * y) :> (k *| ys)

    (u1 :> u2 :> u3 :> Nil) `cross` (v1 :> v2 :> v3 :> Nil) = (u2 * v3 - u3 * v2) :> (u3 * v1 - u1 * v3) :> (u1 * v2 - u2 * v1) :> Nil



instance Append (Vec n a) (Vec m a) where
    type C (Vec n a) (Vec m a) = (Vec (n+m) a)

    (x :> xs) `append` ys = x :> (xs `append` ys)
    Nil `append` ys = ys

instance Functor (Vec n) where

    fmap f (x :> xs) = (f x) :> (fmap f xs)
    fmap _ Nil = Nil
