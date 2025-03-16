isInNumber :: Int -> Int -> Bool
isInNumber 0 0 = True
isInNumber x k
  | x `mod` 10 == k = True
  |otherwise = isInNumber (x `div` 10) k

countsInNumber :: Int -> Int -> Int
countsInNumber x k = countDigits x k 0
countDigits :: Int -> Int -> Int -> Int
countDigits 0 _ sum = sum
countDigits x k sum
  | x `mod` 10 == k = countDigits (x `div` 10) k (sum + 1)
  |otherwise = countDigits (x `div` 10) k sum

main :: IO ()
main = do
   print (countsInNumber 123 3)
