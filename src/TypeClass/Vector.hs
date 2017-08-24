module TypeClass.Vector where

infixr 7 *|
infixl 7 |*
infixl 6 |+|
infixl 6 |-|
infixl 8 |*|
class Vector a where
    type T a
    (|+|) :: (Floating (T a)) => a -> a -> a
    (|-|) :: (Floating (T a)) => a -> a -> a
    dot   :: (Floating (T a)) => a -> a -> T a
    (|*)  :: (Floating (T a)) => a -> T a -> a
    (*|)  :: (Floating (T a)) => T a -> a -> a
    cross :: (Floating (T a)) => a -> a -> a
    (|*|) :: (Floating (T a)) => a -> a -> a

    smag :: (Floating (T a)) => a -> T a
    smag x = x `dot` x

    mag :: (Floating (T a)) => a -> T a
    mag = sqrt . smag
