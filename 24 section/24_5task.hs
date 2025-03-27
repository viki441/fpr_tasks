countChar :: Char -> String -> Int
countChar c s = length [x | x <- s, x == c]

uniqueChars :: String -> String
uniqueChars [] = []
uniqueChars (x:xs)
    | x `elem` xs = uniqueChars xs 
    | otherwise   = x : uniqueChars xs  

histogram :: String -> [(Char, Int)]
histogram s = [(c, countChar c s) | c <- uniqueChars s]

main :: IO ()
main = print (histogram "abracadabra")  
