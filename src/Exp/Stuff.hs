module Exp.Stuff where

quickSort :: (Ord a) => [a] -> [a] --speed is a peace of sh*t
quickSort [] = []
quickSort (x : xs) = let
        smaller = quickSort [y | y <- xs, y <= x]
        bigger = quickSort [y | y <- xs, y > x]
     in
        smaller P.++ [x] P.++ bigger