countDigits :: Integer -> Int
countDigits n
  | n < 10    = 1
  | otherwise = 1 + countDigits (n `div` 10)

main :: IO ()
main = do
print(countDigits 2332)

{-
countDigits :: Int -> Int
countDigits x = extractCount x 0

extractCount :: Int -> Int -> Int
extractCount 0 0 = 1
extractCount x count
  | x `mod` 10 > 0 = extractCount (x `div` 10) (count + 1) 
  |otherwise = count

main :: IO ()
main = do 
  print(countDigits 1244543)


-}
