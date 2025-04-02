uniqueElements :: Eq a => [a] -> [a]
uniqueElements [] = []
uniqueElements (x:xs)
    | x `elem` xs = uniqueElements xs
    | otherwise   = x : uniqueElements xs

countElements  :: Eq a => [a] -> a -> Int
countElements [] _ = 0
countElements (x:xs) y
    | x == y = 1 + countElements xs y
    |otherwise = countElements xs y

encodeL :: Eq a => [a] -> [(Int, a)]
encodeL list =  [ (countElements list b, b) |  b <- uniqueElements list]
    
main :: IO ()
main = do
    print (encodeL [1,1,1,2,3,3,3,4,4,4,5,5,7])
