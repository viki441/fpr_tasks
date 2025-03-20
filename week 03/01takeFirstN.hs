takeFirstN :: [Int] -> Int -> [(Int, Int)]
takeFirstN xs n = [ (a, b) | (a, b) <- zip (take n xs) (drop n xs) ]
