{-countElem :: Eq a => a -> [a] -> Int
countElem _ [] = 0
countElem y (x:xs)
  |y == x = 1 + countElem y xs
  |otherwise = countElem y xs
-}
countElem :: Eq a => a -> [a] -> Int
countElem _ [] = 0
countElem y l
  |y == (head l) = 1 + countElem y (tail l)
  |otherwise = countElem y (tail l)

main :: IO ()
main = do
  print(countElem 3 [1,2,4,3,3,3,3,3,3,4])
