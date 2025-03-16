findLength :: [a] -> Int
findLength [] = 0
findLength (_:xs) = 1 + findLength xs

main :: IO ()
main = do
  print(findLength ["a", "d"])
