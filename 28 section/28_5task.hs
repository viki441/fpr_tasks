cumulativeSum :: [Int] -> [Int]
cumulativeSum [] = []
cumulativeSum (x:xs) = x : zipWith (+) (cumulativeSum (x:xs)) xs

main :: IO ()
main = do
    print(cumulativeSum [1,2,3,4,5])
