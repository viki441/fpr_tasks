indexOf :: Int -> [Int] -> Int
indexOf y (x:xs) = findDigit y (x:xs) 0

findDigit :: Int -> [Int] -> Int -> Int
findDigit _ [] _ = -1
findDigit y (x:xs) sum
  | y == x = sum
  |otherwise = findDigit y xs (sum + 1)
 
main :: IO ()
main = do
  print(indexOf 7 [3,2,1,3,2,1,2,5])
