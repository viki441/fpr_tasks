sumDigits :: Integer -> Int
sumDigits n
  | n < 10    = n
  | otherwise = lastDigit + sumDigits (n `div` 10)
  where
    lastDigit = n `mod` 10

main :: IO ()
main = do
  print(sumDigits 12345)


{-sumDigits :: Int -> Int
sumDigits x = extractsum x 0

extractsum :: Int -> Int -> Int
extractsum 0 0 = 0
extractsum x sum
  | x `mod` 10 > 0 = extractsum (x `div` 10) (sum + lastDigit) 
  |otherwise = sum
  where
    lastDigit = x `mod` 10

main :: IO ()
main = do 
  print(sumDigits 1244543)-}
