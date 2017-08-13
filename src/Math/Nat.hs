module Math.Nat where

data Nat = Zero | Succ Nat

type N1 = Succ Zero
type N2 = Succ N1
type N3 = Succ N2

type family (+) (n :: Nat) (m :: Nat) :: Nat
type instance (+) Zero m = m
type instance (+) (Succ n) m = Succ ((+) n m) -- ... = Succ (Plus m n) would not work

type family (-) (n :: Nat) (m :: Nat) :: Nat
type instance (-) m Zero = m
type instance (-) n (Succ m) = Succ ((-) n m)