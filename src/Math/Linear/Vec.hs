
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

import Math.Nat

import Common

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Unsafe.Coerce
import Data.Kind
import Data.Type.Equality
import Data.Singletons.Prelude.Enum



class UVec a where
  data Vec (n :: Nat) a :: *

  
  vnil :: Vec 0 a
  vcons :: a -> Vec (n :- 1) a -> Vec (n) a


  --slow(direct pattern matching is faster)
  vnonEmpty :: Vec n a -> Either (n :~: (m :+ 1)) (n :~: 0)
  vmatch :: Vec (n :+ 1) a -> (a, Vec n a)


instance (UVec a) => UVec (Vec m a) where
  data Vec n (Vec m a) where
    VNilVec :: Vec 0 (Vec m a)
    VConsVec :: Vec m a -> Vec (n :- 1) (Vec m a) ->
      Vec n (Vec m a)

  vnil = VNilVec
  vcons = VConsVec

  vnonEmpty (VConsVec x xs) = unsafeCoerce $ Left Refl
  vnonEmpty VNilVec = Right Refl

  vmatch (VConsVec x xs) = unsafeCoerce (x,xs)

instance UVec Float where
  data Vec n Float where
    VNilFloat :: Vec 0 Float
    VConsFloat :: {-#UNPACK#-}!Float ->
                              Vec (n :- 1) Float ->
                              Vec (n) Float


  vnil = VNilFloat
  vcons = VConsFloat

  vnonEmpty (VConsFloat x xs) = unsafeCoerce $ Left Refl
  vnonEmpty VNilFloat = Right Refl

  vmatch (VConsFloat x xs) = unsafeCoerce (x, xs)

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
  mag x = sqrt $ smag x
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

  (|*|) :: Vec n Float -> Vec n Float -> Vec n Float
  (x `VConsFloat` xs) |*| (y `VConsFloat` ys) =
    (x * y) `VConsFloat` (xs |*| ys)
  VNilFloat |*| VNilFloat = VNilFloat

  (VConsFloat u1 (VConsFloat u2 (VConsFloat u3 VNilFloat)))
    `cross` (VConsFloat v1 (VConsFloat v2 (VConsFloat v3 VNilFloat))) =
    (u2 * v3 - u3 * v2) `VConsFloat`
    ((u3 * v1 - u1 * v3) `VConsFloat`
    ((u1 * v2 - u2 * v1) `VConsFloat` VNilFloat))


  ortho (VConsFloat x (VConsFloat y VNilFloat)) =
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
vpreplicate :: (UVec a) => SNat n -> a -> Vec n a
vpreplicate n val = case isZero n of
  NonZero -> val `vcons` vpreplicate (sPred n) val
  Zero -> vnil



vpforeach :: (UVec a) => (a -> b -> IO b) -> Vec n a -> b -> IO b
vpforeach f v b =
    case vnonEmpty v of
      Left Refl -> do
        let (a,as) = vmatch v
        r <- f a b
        vpforeach f as r
      Right Refl -> pure b


vec2 :: (UVec a) => a -> a -> Vec 2 a
vec2 a b = vcons a (vcons b vnil)

vec3 :: (UVec a) => a -> a -> a -> Vec 3 a
vec3 a b c = vcons a (vcons b (vcons c vnil))

vec4 :: (UVec a) => a -> a -> a -> a -> Vec 4 a
vec4 a b c d = vcons a (vcons b (vcons c (vcons d vnil)))



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

