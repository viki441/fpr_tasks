packL :: (Eq a) => [a] -> [[a]]
packL [] = []
packL (x:xs) = (x : takeWhile (== x) xs) : packL (dropWhile (== x) xs)

main :: IO ()
main = do
    print (packL [1,1,1,2,3,3,3,4,4,4,5,5,7])
