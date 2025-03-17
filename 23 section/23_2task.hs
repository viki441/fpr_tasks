countElem :: Eq a => a -> [a] -> Int
countElem _ [] = 0
countElem y (x:xs)
  |y == x = 1 + countElem y xs
  |otherwise = countElem y xs

main :: IO ()
main = do
  print(countElem 3 [1,2,4,3,3,3,3,3,3,4])
