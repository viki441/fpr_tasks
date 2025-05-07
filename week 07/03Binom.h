factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

data Binom = Binom {n :: Int, k :: Int}
    deriving(Show, Eq)

binomialCoefficient :: Binom -> Int
binomialCoefficient (Binom n k)
    | k < 0 || k > n = 0
    | otherwise = factorial n `div` (factorial k * factorial(n - k))

main :: IO ()
main = do
    let b = Binom 5 2
    let res = binomialCoefficient b
    putStrLn("Binomal coefficient (n choose k) is: " ++ show res)
