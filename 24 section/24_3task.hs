--diff way
makeList :: Int -> [Int]
makeList n = [read [c] | c <- reverse(show n)]

reverseDigits :: Int -> [Int]
reverseDigits x = reverse(removeDuplicates [] (makeList x))

removeDuplicates :: [Int] -> [Int] -> [Int]
removeDuplicates list [] = list
removeDuplicates list (y:ys)
    | y `elem` list = removeDuplicates list ys 
    | otherwise   = removeDuplicates (y:list) ys 


main :: IO ()
main = do
    print (reverseDigits 37182931943180)
