findByIndex :: [Int] -> [Int]
findByIndex (x:xs) = zipWith (+) (x:xs) xs
main :: IO ()
main = do
    print (findByIndex [1,2,3,4,3,2])
