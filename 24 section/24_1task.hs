firstNEvens :: Int -> [Int]
firstNEvens n = take n [x | x <- [2..], x `mod` 2 == 0]

main :: IO ()
main = do
    print (firstNEvens 10)  
