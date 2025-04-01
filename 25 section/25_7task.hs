combine :: [Int] -> [(Int, Int)]
combine [] = []
combine l = [(a, b) | (a, b) <- zip l (tail l), a < b]

main :: IO ()
main = do
    print (combine [1, 3, 2, 4, 6, 5])  
