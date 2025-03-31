--solution 1
countCommon :: Eq a => [a] -> [a] -> Int -> Int
countCommon [] _ sum = sum
countCommon _ [] sum = sum
countCommon (x:xs) (y:ys) sum
    | x == y = countCommon xs ys (sum + 1)
    | otherwise = countCommon xs ys sum

countPrefix :: Eq a => [a] -> [a] -> Int
countPrefix [] _ = 0
countPrefix _ [] = 0
countPrefix l1 l2 = countCommon l1 l2 0


--solution 2
longestCommonPrefix :: Eq a => [a] -> [a] -> Int
longestCommonPrefix xs ys = length (takeWhile (uncurry (==)) (zip xs ys))
