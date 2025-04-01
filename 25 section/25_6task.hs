--solution 1
checkElements :: String -> Int -> Int -> Bool
checkElements list ind1 ind2
    | ind1 >= ind2 = True
    | (list !! ind1) /= (list !! ind2) = False
    | otherwise = checkElements list (ind1 + 1) (ind2 - 1)

palindrome:: String -> Bool
palindrome list = checkElements list 0 (length list-1)

--solution 2
palindrome1 :: String -> Bool
palindrome1 str = str == reverse str

main :: IO ()
main = do
    print (palindrome "yaly")
