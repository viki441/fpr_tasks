sumWithPartner :: [Int] -> [Int]
sumWithPartner (x:xs) = zipWith (+) (x:xs) xs
main :: IO (
main = do
    print (sumWithPartner [1,2,3,4,3,2])
