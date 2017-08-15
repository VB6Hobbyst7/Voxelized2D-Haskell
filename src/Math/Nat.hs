module Math.Nat where

import Data.Proxy

data Nat = Zero | Succ Nat

type N0 = Zero
type N1 = Succ Zero
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3

n0 = Z
n1 = S n0
n2 = S n1
n3 = S n2
n4 = S n3
n5 = S n4

data SNat n where
  Z :: SNat Zero
  S :: SNat n -> SNat (Succ n)

(%:+) :: SNat n -> SNat m -> SNat (n + m)
Z   %:+ m = m
S n %:+ m = S (n %:+ m)

(%:-) :: SNat n -> SNat m -> SNat (n - m)
n   %:- Z = n
(S n) %:- S m = n %:- m

type family (+) (n :: Nat) (m :: Nat) :: Nat
type instance (+) Zero m = m
type instance (+) (Succ n) m = Succ ((+) n m) -- ... = Succ (Plus m n) would not work

type family (-) (n :: Nat) (m :: Nat) :: Nat
type instance (-) n Zero = n
type instance (-) (Succ n) (Succ m) = (-) n m
