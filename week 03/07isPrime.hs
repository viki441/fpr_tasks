isPrime :: Int -> Bool
isPrime x = null [d | d <- [2..(x-1)], x `mod` d == 0]

firstNPrimes :: Int -> [Int]
firstNPrimes n = take n [x | x <- [2..], isPrime x]

main :: IO ()
main = do
    print (firstNPrimes 10)  
