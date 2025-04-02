
mergeEvenOdd :: [a] -> [a] -> [a]
mergeEvenOdd [] [] = []
mergeEvenOdd (x:xs) [] = [x]
mergeEvenOdd [] (y:ys) = [y]
mergeEvenOdd (x:xs) (y:ys) = y : x : mergeEvenOdd xs ys

main :: IO ()
main = do
    print (mergeEvenOdd[1, 2, 5, 9, 16] [2,3,4,1])
