module AlgObj where

class AddMonoid a where
  plus :: a -> a -> a
  zero :: a

class MulMonoid a where
  mult :: a -> a -> a
  one :: a -> a -> a

class (AddMonoid a) => ComAddGroup a where
  negate :: a -> a
  minus :: a -> a -> a

class (ComAddGroup a, MulMonoid a) => Ring a where


class (Ring a) => Field a
  inv :: a -> a
  div :: a -> a -> a
