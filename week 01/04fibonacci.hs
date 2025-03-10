fibonacci :: Int -> Int
fibonacci n 
    | n < 0     = -1        
    | n == 0    = 0        
    | n == 1    = 1         
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)  

main :: IO ()
main = do
    print (fibonacci 10)
