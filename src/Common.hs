module Common where


import Timer
import Data.List

infixl 0 |>
(|>) :: a -> (a -> b) -> b
a |> f = f $! a

println :: String -> IO ()
println a = putStrLn a

int :: Int -> Int
int a = a


for :: a -> (a -> Bool) -> (a -> a) -> b -> (a -> b -> IO b ) -> IO b
for val pred step initial body
    | pred val = do
        !res <- body val initial
        for (step val) pred step res body
    | otherwise = pure initial



timed :: (Integer -> String) -> IO a -> IO a
timed howToRender op = do
      !t1 <- timeMs
      !res <- op
      !t2 <- timeMs

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


removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates = Data.List.foldl (\seen x -> if x `elem` seen
                then seen
                else seen ++ [x]) []
