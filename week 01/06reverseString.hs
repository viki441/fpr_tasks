reverseString :: [Char] -> [Char]
reverseString [] = []
reverseString [x] = [x]
reverseString (x:xs) = reverseString xs ++ [x]

main :: IO ()
main = do 
  print(reverseString ['e','e','t','f'])
