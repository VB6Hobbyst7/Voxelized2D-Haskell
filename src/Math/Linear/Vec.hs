
{-#LANGUAGE
TypeInType,
GADTs,
TypeOperators,
DataKinds,
TemplateHaskell,
TypeFamilies,
UndecidableInstances,
ScopedTypeVariables,
TypeFamilies,
KindSignatures,
FlexibleContexts,
RankNTypes,
FlexibleInstances,
InstanceSigs,
DefaultSignatures,
AllowAmbiguousTypes
#-}

module Math.Linear.Vec where

--import Math.Nat
import TypeClass.Vector
import TypeClass.Append
import Common

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Unsafe.Coerce

infixr 8 :>
data  Vec :: Nat -> * -> * where   --same as : data List (n :: Nat) a where ...
  Nil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a

class UVec a where
  data Vec (n :: Nat) a :: *

  --slow:
  vnil :: Vec 0 a
  vcons :: a -> Vec n a -> Vec (n :+ 1) a

  vnonEmpty :: Vec n a -> Either (n :~: (m :+ 1)) (n :~: 0)
  vmatch :: Vect (n :+ 1) a -> (a, Vec n a)
  

instance UVec Float where
  data Vec n Float where
    VNilFloat :: Vec 0 Float
    VConsFloat :: {-#UNPACK#-}!Float ->
                              Vec n Float ->
                              Vec (n :+ 1) Float


  vnil = VNilFloat
  vcons = VConsFloat

  vnonEmpty (VConsFloat x xs) = unsafeCoerce $ Left Refl
  vnonEmpty VNilFloat = Right Refl

  vmatch (VConsFloat x xs) = (x, xs)

infixl 4 |+|
infixl 4 |-|
infixl 6 |*
infixr 6 *|
--infixl 6 |/
--infixr 6 /|
infixl 6 |*|

class (UVec a) => AlgVec a where
  (|+|) :: (Num a) => Vec n a -> Vec n a -> Vec n a
  (|-|) :: (Num a) => Vec n a -> Vec n a -> Vec n a
  (|*|) :: (Num a) => Vec n a -> Vec n a -> Vec n a
  (|*)  :: (Num a) => Vec n a -> a -> Vec n a
  (*|)  :: (Num a) => a -> Vec n a -> Vec n a
  dot   :: (Num a) => Vec n a -> Vec n a -> a
  smag  :: (Num a) => Vec n a -> a
  smag x = dot x x
  mag   :: (Floating a) => Vec n a -> a
  mag x = sqrt $ mag x
  cross :: (Num a) => Vec 3 a -> Vec 3 a -> Vec 3 a
  ortho :: (Num a) => Vec 2 a -> Vec 2 a

  x :: (n ~ (m :+ 1)) => Vec n a -> a
  y :: (n ~ (m :+ 2)) => Vec n a -> a
  z :: (n ~ (m :+ 3)) => Vec n a -> a


instance AlgVec Float where

  (VConsFloat x xs) |+| (VConsFloat y ys) =
    (x + y) `VConsFloat` (xs |+| ys)
  VNilFloat |+| ys = ys

  (x `VConsFloat` xs) |-| (y `VConsFloat` ys) =
    (x - y) `VConsFloat` (xs |-| ys)
  VNilFloat |-| ys = ys

  (x `VConsFloat` xs) `dot` (y `VConsFloat` ys) =
    (x * y) + (dot xs ys)
  VNilFloat `dot` VNilFloat = 0

  (x `VConsFloat` xs) |* k =
    (x * k) `VConsFloat` (xs |* k)
  VNilFloat |* k = VNilFloat

  k *| (y `VConsFloat` ys) =
    (k * y) `VConsFloat` (k *| ys)
  k *| VNilFloat = VNilFloat

  (x `VConsFloat` xs) |*| (y `VConsFloat` ys) =
    (x * y) `VConsFloat` (xs |*| ys)
  VNilFloat |*| VNilFloat = VNilFloat

  (VConsFloat u1 (VConsFloat u2 (VConsFloat u3 VNilFloat)))
    `cross` (VConsFloat v1 (VConsFloat v2 (VConsFloat v3 VNilFloat))) =
    (u2 * v3 - u3 * v2) `VConsFloat`
    (u3 * v1 - u1 * v3) `VConsFloat`
    (u1 * v2 - u2 * v1) `VConsFloat` VNilFloat


  ortho (VConsFloat x (VConsFloat y VNilFloat) =
         VConsFloat (-y) (VConsFloat x VNilFloat)

  x (VConsFloat a _) = a
  y (VConsFloat _ (VConsFloat b _)) = b
  z (VConsFloat _ (VConsFloat _ (VConsFloat c _))) = c
        

type Vec2 a = Vec 2 a
type Vec3 a = Vec 3 a
type Vec4 a = Vec 4 a

type Vec2F = Vec2 Float
type Vec3F = Vec3 Float
type Vec4F = Vec4 Float

--polimorphic replicate
{-vpreplicate :: SNat n -> a -> Vec n a
vpreplicate (S n) val = val :> vpreplicate n val
vpreplicate Z _ = Nil-}


vpforeach :: (a -> b -> IO b) -> Vec n a -> b -> IO b
vpforeach f v b =
    case vnonEmpty v of
      Left Refl -> do
        let (a,as) = vmatch v
        r <- f a b
        vpforeach f as r
      Right Refl -> b


vec2 :: a -> a -> Vec N2 a
vec2 a b = a :> b :> Nil

vec3 :: a -> a -> a -> Vec N3 a
vec3 a b c = a :> b :> c :> Nil

vec4 :: a -> a -> a -> a -> Vec N4 a
vec4 a b c d = a :> b :> c :> d :> Nil



instance (UVec a, Show a) => Show (Vec n a) where

    show :: Vec n a -> String
    show a =
      case vnonEmpty a of
        Left Refl -> "[" ++ show1 a ++ "]"
        Right Refl -> "[]"
      where
        show1 :: Vec (m :+ 1) a -> String
        show1 a =
          let
            (x,xs) = vmatch a
          in
            case vnonEmpty xs of
              Left Refl -> show x ++ ", " ++ (show1 xs)
              Right Refl -> show x

