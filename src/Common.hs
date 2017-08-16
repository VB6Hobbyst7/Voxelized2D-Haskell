{-# LANGUAGE Strict #-}

module Common where


import Timer
import Data.List
import Data.Array.IO

infixl 0 |>
(|>) :: a -> (a -> b) -> b
a |> f = f a

println :: String -> IO ()
println = putStrLn

int :: Int -> Int
int a = a


for :: a -> (a -> Bool) -> (a -> a) -> b -> (a -> b -> IO b ) -> IO b
for val pred step initial body
    | pred val = do
        res <- body val initial
        for (step val) pred step res body
    | otherwise = pure initial

for' :: a -> (a -> Bool) -> (a -> a) -> (a -> IO b ) -> IO ()
for' val pred step body
    | pred val = do
        res <- body val
        for' (step val) pred step body
    | otherwise = pure ()

timed :: (Integer -> String) -> IO a -> IO a
timed howToRender op = do
      t1 <- timeMs
      res <- op
      t2 <- timeMs

      println $ howToRender (t2 - t1)

      pure res

while :: (a -> IO Bool) -> a -> (a -> IO a) -> IO a
while predicate x func = do
      ok <- predicate x
      if ok
          then
              do
                    y <- func x
                    while predicate y func
          else
              return x

swap :: IOArray Int a -> Int -> Int -> IO ()
swap arr i j = do
  atI <- readArray arr i
  atJ <- readArray arr j
  writeArray arr i atJ
  writeArray arr j atI

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates = Data.List.foldl (\seen x -> if x `elem` seen
                then seen
                else seen ++ [x]) []

facSort :: IOArray Int Int -> IO ()
facSort arr = do
  len <- (+ 1) . snd <$> getBounds arr
  for' 0 (< len - 1) (+ 1) $ \i ->
    for' (i + 1) (< len) (+ 1) $ \j -> do
      this <- readArray arr i
      next <- readArray arr j
      if this > next
        then swap arr i j
        else pure ()