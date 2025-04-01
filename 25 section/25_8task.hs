groupsOfL :: [Int] -> Int -> [[Int]]
groupsOfL [] _ = []
groupsOfL list n = (take n list) : (groupsOfL (drop n list) n)

main :: IO ()  
main = do
    print (groupsOfL [1, 3, 2, 4, 6, 5] 4)  
