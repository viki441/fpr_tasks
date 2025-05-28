insertIntoBucket :: Eq a => a -> [[a]] -> [[a]]
insertIntoBucket x [] = [[x]]
insertIntoBucket x (ys:yss)
    | x `elem` ys = ys : insertIntoBucket x yss
    | otherwise   = (x:ys) : yss

minimalSplit :: Eq a => [a] -> [[a]]
minimalSplit = foldr insertIntoBucket []


main :: IO ()
main = do
    print(minimalSplit [1,3,4,2,4,5,5,3,2,4,1,3,4])
