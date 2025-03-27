--A
firstNEvens :: Int -> [Int]
firstNEvens n = take n [x | x <- [2..], x `mod` 2 == 0]
--B
arProgession :: Int -> Int -> Int -> [Int]
arProgession n a d = [a + (i * d) | i <- [0..(n-1)]]
--C
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

permutation :: Int -> [Int]
permutation n = [factorial x | x <-[1..n]]
--D
isEven :: Int -> Bool
isEven x
    | x `mod` 2 == 0 = True
    |otherwise = False

allEven :: Int -> [Int]
allEven x = [n | n <- [1..x], isEven n]
--E
allEvens :: [Int]
all Evens = [x | x <- [2..], x `mod` 2 == 0]
--can call take 2 allEvens

--F
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

permutation :: [Int]
permutation = [factorial x | x <-[1..]]
--can call take 3 permutation

main :: IO ()
main = do
    print (firstNEvens 10)  
