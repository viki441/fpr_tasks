pow :: Int -> Int -> Int
-- x k = x ^ k
pow 0 _ = 1
pow x k 
  | k > 1  = x * pow x (k-1)
  | otherwise = x

main :: IO ()
main = do

  print(pow 2 10)
