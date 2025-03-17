findIndex :: Eq a => a -> [a] -> Int
findIndex y (x:xs) = findNumber y (x:xs) 0
findNumber :: Eq a => a -> [a] -> Int -> Int
findNumber _ [] 0 = -1
findNumber y (x:xs) sum
  | y == x = sum
  | otherwise = findNumber y xs (sum + 1)

main :: IO ()
main = do
  print(findIndex 3 [1,2,4,5,5,5,3,3,3,3,3,4])
