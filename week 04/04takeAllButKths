takeAllButKths :: [a] -> Int -> [a]
takeAllButKths list k = [x | (x, i) <- zip list [0..], (i+1) `mod` k /= 0]
main :: IO ()
main = do
  print(takeAllButKths [2,3,4,5,6,7,8,9,10] 3)
