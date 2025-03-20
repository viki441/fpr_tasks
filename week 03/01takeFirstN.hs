takeFirstN :: [Int] -> Int -> [(Int, Int)]
takeFirstN xs n = [ (a, b) | (a, b) <- zip (take n xs) (drop n xs) ]

main :: IO ()
main = do
  print(takeFirstN [1,2,3,4,5,6] 3 )
