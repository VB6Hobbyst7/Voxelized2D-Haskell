module Math.Linear.Vect where

import Math.Nat
import TypeClass.Vector
import TypeClass.Append

infixr 8 :>
data  Vect :: Nat -> * -> * where   --same as : data List (n :: Nat) a where ...
  Nil :: Vect Zero a
  (:>) :: a -> Vect n a -> Vect (Succ n) a

instance (Show a) => Show (Vect n a) where
    show Nil = "Nil"
    show (x :> xs) = (show x) ++ " :> " ++ (show xs)

instance (Floating a) => Vector (Vect n a) where
    type T (Vect n a) = a

    (x :> xs) |+| (y :> ys) = (x + y) :> (xs |+| ys)
    Nil |+| ys = ys

    (x :> xs) |-| (y :> ys) = (x - y) :> (xs |-| ys)
    Nil |-| ys = ys

    (x :> xs) `dot` (y :> ys) = (x * y) + (dot xs ys)
    Nil `dot` Nil = 0

    (x :> xs) |* k = (x * k) :> (xs |* k)
    k *| (y :> ys) = (k * y) :> (k *| ys)

    (u1 :> u2 :> u3 :> Nil) `cross` (v1 :> v2 :> v3 :> Nil) = (u2 * v3 - u3 * v2) :> (u3 * v1 - u1 * v3) :> (u1 * v2 - u2 * v1) :> Nil



instance Append (Vect n a) (Vect m a) where
    type C (Vect n a) (Vect m a) = (Vect (n+m) a)

    (x :> xs) `append` ys = x :> (xs `append` ys)
    Nil `append` ys = ys

instance Functor (Vect n) where

    fmap f (x :> xs) = (f x) :> (fmap f xs)
    fmap _ Nil = Nil