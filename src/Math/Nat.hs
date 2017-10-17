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
StandaloneDeriving,
TypeApplications
#-}

module Math.Nat where

import Data.Proxy
import Data.Singletons.TypeLits
import GHC.TypeLits ( type (<=), type (-) )
import Data.Type.Equality
import Unsafe.Coerce
import Data.Singletons.Decide


data IsZero (n :: Nat)
  where Zero :: (0 ~ n) => IsZero n
        NonZero :: (1 <= n) => IsZero n
deriving instance Show (IsZero n)

isZero :: forall n. SNat n -> IsZero n
isZero n = case n %~ (SNat @0) of
  Proved Refl -> Zero
  Disproved _ -> unsafeCoerce (NonZero @1)
