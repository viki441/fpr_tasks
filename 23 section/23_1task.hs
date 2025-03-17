findLength :: [a] -> Int
findLength [] = 0
findLength (_:xs) = 1 + findLength xs

findElem :: Eq a => a -> [a] -> Bool
findElem _ [] = False
findElem n (x:xs)
  | n == x = True
  |otherwise = findElem n xs

countElem :: Num a => [a] -> a
countElem [] = 0
countElem (x:xs) = x + countElem xs

main :: IO ()
main = do
  print(findLength ["a", "d"])
  print(findElem 5 [1,2,3,4])
  print(countElem [1,2,3,4])
