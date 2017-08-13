module TypeClass.Vector where

infixr 8 *|
infixl 8 |*
class Vector a where
    type T a
    (|+|) :: (Floating (T a)) => a -> a -> a
    (|-|) :: (Floating (T a)) => a -> a -> a
    dot   :: (Floating (T a)) => a -> a -> T a
    (|*)  :: (Floating (T a)) => a -> (T a) -> a
    (*|)  :: (Floating (T a)) => (T a) -> a -> a
    cross :: (Floating (T a)) => a -> a -> a

    mag :: (Floating (T a)) => a -> T a
    mag x = (x `dot` x) ** 0.5