
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
AllowAmbiguousTypes,
TypeApplications
#-}

module Math.Linear.Mat where


import Math.Linear.Vec
import qualified Math.Linear.Vec as Vec
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Enum
import Data.Type.Equality
import Math.Nat
import GHC.TypeLits
import Data.Singletons.Decide
import Unsafe.Coerce

type Mat n m a = Vec n (Vec m a)

mat4 :: (UVec a) => Vec 4 a -> Vec 4 a -> Vec 4 a -> Vec 4 a -> Mat 4 4 a -- 
mat4 a b c d = vcons a (vcons b (vcons c (vcons d vnil)))
  

--                  0 1 2 3 4 ... n-1
--returns Vec n a : 1 0 0 0 0 ... 0
helper :: (UVec a, Num a) => SNat n -> Vec n a
helper n = case isZero n of
  NonZero -> 1 `vcons` vpreplicate (sPred n) 0
  Zero -> vnil


--remove all singletons, set proxies (cause they are prob faster)
proof1 :: forall n m . (CmpNat n m ~ GT) => SNat n -> SNat m -> ((CmpNat (n :- 1) (m :- 1)) :~: GT)  --n > m => (n-1) > (m-1)
proof1 n m = unsafeCoerce Refl

proof2 :: forall n m . (1 <= m, (CmpNat n m) ~ GT) => SNat n -> SNat m -> ((1 <=? (n :- 1)) :~: True)--1 <= m < n => 1 <= n - 1
proof2 n m = unsafeCoerce Refl

proof3 :: forall n m. SNat n -> SNat m -> ((CmpNat n (n :- m)) :~: GT)                               --n <= n - m
proof3 _ _ = unsafeCoerce Refl

proof4 :: forall n m. (n <= m) => SNat n -> SNat m -> (((n :- 1) <=? m) :~: True)                    --n <= m => n-1 <= m
proof4 _ _ = unsafeCoerce Refl

--                  0 1 2 ... m . . . . . . n-1
--returns Vec n a : 0 0 0 ... 1 0 0 0 0 ... 0
helper2 :: forall a m n.(UVec a, Num a, 1 <= n, (CmpNat n m) ~ GT) => SNat n -> SNat m -> Vec n a
helper2 size m = case (isZero m, proof1 size m) of
  (NonZero, Refl) ->
    case (proof2 size m) of
      Refl -> (0 :: a) `vcons` helper2 (sPred size) (sPred m)
      
  (Zero,_) -> helper size


identityHelper :: forall a n m. (UVec a, Num a, n <= m, 1 <= m) => SNat n -> SNat m -> Mat n m a
identityHelper n m = case (proof3 m (SNat @1), proof3 m n, proof4 n m) of
    (Refl,Refl,Refl) ->
     case n %~ (SNat @1) of
     Proved Refl -> helper2 m (m %:- n) `vcons` vnil
     Disproved _ -> helper2 m (m %:- n) `vcons` identityHelper (sPred n) m

--return identity matrix of particular size
identity :: (UVec a, Num a, 1 <= n) => SNat n -> Mat n n a
identity n = identityHelper n n


mpforeach :: (UVec a) => (a -> b -> IO b) -> Mat n m a -> b -> IO b
mpforeach f v b =
    case vnonEmpty v of
      Left Refl -> do
        let (a,as) = vmatch v
        r <- vpforeach f a b
        mpforeach f as r
      Right Refl -> pure b




--column major (feed to OpenGL without tranposing)
ortho :: Float -> Float -> Float -> Float -> Float -> Float -> Mat 4 4 Float
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
