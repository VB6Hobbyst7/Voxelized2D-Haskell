module TypeClass.Append where

class Append a b where
    type C a b
    append :: a -> b -> C a b