prod :: [Int] -> Int
prod [] = 1
prod [x] = x
prod (x:xs) = x * prod xs

main :: IO ()
main = do
    print(prod [1,2,3,-1])
