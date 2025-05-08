findByIndex :: [Int] -> [Int]
findByIndex list = [x | (i, x) <- zip [1..] list, i == x]
main :: IO ()
main = do
    print (findByIndex [1,2,3,4,3,2])
