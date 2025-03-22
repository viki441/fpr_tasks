makePairs :: [Int] -> [Int] -> [(Int, Int)]
makePairs xs ys = [(x, y) | x <- xs, y <- ys]

main :: IO ()
main = do
    print(makePairs [1,2] [3,4])
