firstNEvens :: Int -> [Int]
firstNEvens n = take n [x | x <- [2..], x `mod` 2 == 0]

arProgession :: Int -> Int -> Int -> [Int]
arProgession n a d = [a + (i * d) | i <- [0..(n-1)]]


main :: IO ()
main = do
    print (firstNEvens 10)  
