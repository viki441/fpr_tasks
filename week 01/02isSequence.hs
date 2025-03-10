reverseNumber :: Int -> Int
reverseNumber n = helper n 0
  where
    helper 0 reversed = reversed
    helper num reversed = helper (num `div` 10) (reversed * 10 + num `mod` 10)

absoluteReversed :: Int -> Int
absoluteReversed x
    | x >= 0    = x  
    | otherwise = reverseNumber (-x)

numberToList :: Int -> [Int]
numberToList 0 = []
numberToList n = numberToList (n `div` 10) ++ [n `mod` 10]

checkList :: [Int] -> Bool
checkList [] = True
checkList [_] = True
checkList (x:y:xs) = if x < y then checkList (y:xs) else False


main :: IO ()
main = do
    print (checkList(numberToList (absoluteReversed (123))))
