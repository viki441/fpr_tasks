isPerfectNumber :: Int -> Bool
isPerfectNumber n = sumDivisors n 1 0 == n

sumDivisors :: Int -> Int -> Int -> Int
sumDivisors n currentDivisor sum
  | currentDivisor == n = sum  
  | n `mod` currentDivisor == 0 = sumDivisors n (currentDivisor + 1) (sum + currentDivisor) 
  | otherwise = sumDivisors n (currentDivisor + 1) sum 

perfectNumber :: Int -> [Int]
perfectNumber n = [x | x <- [2..n] , isPerfectNumber x]

main :: IO ()
main = do
    print (perfectNumber 120)
