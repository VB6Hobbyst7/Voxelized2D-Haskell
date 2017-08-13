module Common where

infixl 0 |>
(|>) :: a -> (a -> b) -> b
a |> f = f $! a

println :: String -> IO ()
println a = putStrLn a

int :: Int -> Int
int a = a