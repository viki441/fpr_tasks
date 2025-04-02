greaterThanSumRecursive :: (Num a, Ord a) => [a] -> [a]
greaterThanSumRecursive list = checkSum 0 list

checkSum :: (Num a, Ord a) => a -> [a] -> [a]
checkSum _ [] = []
checkSum sumBefore (x:xs)
    | x > sumBefore = x : checkSum (sumBefore + x) xs
    | otherwise = checkSum (sumBefore + x) xs


main :: IO ()
main = do
    print (greaterThanSumRecursive [1, 2, 5, 9, 16])


