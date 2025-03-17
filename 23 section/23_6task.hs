duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates [_] = False
duplicates (x:xs)
  | x `elem` xs = True
  |otherwise = duplicates xs

main :: IO ()
main = do
  print(duplicates[1,2,3,4,5])
