isPower :: Int -> Int -> Bool
isPower 1 _ = True
isPower n k
  | n < 1 || k <= 1 = False 
  | n `mod` k /= 0 = False
  | otherwise = isPower (n `div` k) k

main :: IO ()
main = do 
  print(isPower 1 2)
