makeList :: (Num a) => Int -> [a] -> [[a]]
makeList num list
    | length list < num = []
    |otherwise = take num list : makeList num (tail list)


compareMultiplications :: (Num a, Ord a) => [[a]] -> a
compareMultiplications [] = 0
compareMultiplications [x] = product x
compareMultiplications (x:y:xs)
    | product x > product y = compareMultiplications (y:xs)
    | otherwise = compareMultiplications (x:xs)

minWindowProduct :: (Num a, Ord a) => [a] -> Int -> a
minWindowProduct [] _ = 0
minWindowProduct list x = compareMultiplications (makeList x list)


main :: IO ()
main = do
    print(minWindowProduct [1,2,3,-2,6,-1] 3)
